use std::cell::RefCell;
use std::collections::{hash_map::Entry, HashMap};
use std::ffi::CString;
use std::rc::Rc;

use libc;
// use cpuprofiler::PROFILER;

use builtin;
use bytecode_gen::{ByteCode, VMInst};
use jit::TracingJit;
use node::BinOp;

pub type RawStringPtr = *mut libc::c_char;

pub unsafe fn alloc_rawstring(s: &str) -> RawStringPtr {
    let p = libc::calloc(1, s.len() + 2) as RawStringPtr;
    libc::strncpy(p, s.as_ptr() as *const i8, s.len());
    p
}

pub type CallObjectRef = Rc<RefCell<CallObject>>;

#[derive(Clone, Debug, PartialEq)]
pub struct CallObject {
    pub vals: Rc<RefCell<HashMap<String, Value>>>,
    pub params: Vec<(String, bool)>, // (name, rest param?)
    pub arg_rest_vals: Vec<Value>,
    pub this: Box<Value>,
    pub parent: Option<CallObjectRef>,
}

impl CallObject {
    pub fn new(this: Value) -> CallObject {
        CallObject {
            vals: Rc::new(RefCell::new(HashMap::new())),
            params: vec![],
            arg_rest_vals: vec![],
            this: Box::new(this),
            parent: None,
        }
    }

    pub fn new_global() -> CallObjectRef {
        let vals = Rc::new(RefCell::new(HashMap::new()));
        let callobj = Rc::new(RefCell::new(CallObject {
            vals: vals.clone(),
            params: vec![],
            arg_rest_vals: vec![],
            this: Box::new(Value::Undefined),
            parent: None,
        }));
        *callobj.borrow_mut().this = Value::Object(vals);
        callobj
    }

    pub fn set_value(&mut self, name: String, val: Value) {
        self.vals.borrow_mut().insert(name, val);
    }

    pub fn set_value_if_exist(&mut self, name: String, val: Value) {
        match self.vals.borrow_mut().entry(name.clone()) {
            Entry::Occupied(ref mut v) => *v.get_mut() = val,
            Entry::Vacant(v) => {
                match self.parent {
                    Some(ref parent) => return parent.borrow_mut().set_value_if_exist(name, val),
                    None => v.insert(val),
                };
            }
        }
    }

    pub fn get_value(&self, name: &String) -> Value {
        if let Some(val) = self.vals.borrow().get(name) {
            return val.clone();
        }
        match self.parent {
            Some(ref parent) => return parent.borrow().get_value(name),
            None => panic!("variable not found '{}'", name),
        }
    }

    pub fn get_arguments_nth_value(&self, n: usize) -> Value {
        if n < self.params.len() {
            let param_name = &self.params[n].0;
            return self.get_value(param_name);
        }

        let n = n - self.params.len();
        if n >= self.arg_rest_vals.len() {
            return Value::Undefined;
        }
        self.arg_rest_vals[n].clone()
    }

    pub fn set_arguments_nth_value(&mut self, n: usize, val: Value) {
        if n < self.params.len() {
            let param_name = self.params[n].0.clone();
            self.set_value(param_name, val);
            return;
        }

        let n = n - self.params.len();
        if n >= self.arg_rest_vals.len() {
            return;
        }
        self.arg_rest_vals[n] = val;
    }

    pub fn get_arguments_length(&self) -> usize {
        self.params.len() + self.arg_rest_vals.len()
    }

    pub fn get_parameter_nth_name(&self, n: usize) -> Option<String> {
        if n < self.params.len() {
            return Some(self.params[n].0.clone());
        }
        None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayValue {
    pub elems: Vec<Value>,
    pub length: usize,
    pub obj: HashMap<String, Value>,
}

impl ArrayValue {
    pub fn new(arr: Vec<Value>) -> ArrayValue {
        let len = arr.len();
        ArrayValue {
            elems: arr,
            length: len,
            obj: {
                let mut hm = HashMap::new();
                hm.insert(
                    "__proto__".to_string(),
                    Value::Object(Rc::new(RefCell::new({
                        let mut hm = HashMap::new();
                        hm.insert(
                            "push".to_string(),
                            Value::BuiltinFunction(
                                builtin::ARRAY_PUSH,
                                CallObject::new(Value::Undefined),
                            ),
                        );
                        hm
                    }))),
                );
                hm
            },
        }
    }

    pub fn to_string(&self) -> String {
        self.elems[0..self.length].iter().fold("".to_string(), |acc, val|{
            acc + val.to_string().as_str()
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Undefined,
    Bool(bool),
    Number(f64),
    String(CString),
    Function(usize, Rc<RefCell<HashMap<String, Value>>>, CallObject),
    BuiltinFunction(usize, CallObject),          // id(==0:unknown)
    Object(Rc<RefCell<HashMap<String, Value>>>), // Object(HashMap<String, Value>),
    Array(Rc<RefCell<ArrayValue>>),
    Arguments,
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Bool(b) => {
                if *b {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            Value::Number(n) => {
                if n.is_nan() {
                    return "NaN".to_string();
                }

                if *n == 0.0 {
                    return "0".to_string();
                }

                if n.is_infinite() {
                    return "Infinity".to_string();
                }

                // TODO: Need a correct implementation!
                //  ref. https://tc39.github.io/ecma262/#sec-tostring-applied-to-the-number-type
                format!("{}", *n)
            }
            Value::String(s) => s.clone().into_string().unwrap(),
            Value::Array(ary_val) => ary_val.borrow().to_string(),
            e => unimplemented!("{:?}", e),
        }
    }
}

pub fn new_value_function(pos: usize, callobj: CallObject) -> Value {
    let mut val = Value::Function(
        pos,
        Rc::new(RefCell::new({
            let mut hm = HashMap::new();
            hm.insert(
                "prototype".to_string(),
                Value::Object(Rc::new(RefCell::new(HashMap::new()))),
            );
            hm.insert(
                "__proto__".to_string(),
                Value::Object(Rc::new(RefCell::new({
                    let mut hm = HashMap::new();
                    hm.insert(
                        "call".to_string(),
                        Value::BuiltinFunction(
                            builtin::FUNCTION_PROTOTYPE_CALL,
                            CallObject::new(Value::Undefined),
                        ),
                    );
                    hm
                }))),
            );
            hm
        })),
        callobj,
    );

    let v2 = val.clone();
    if let Value::Function(_, ref mut obj, _) = &mut val {
        // TODO: Add constructor of this function itself (==Function). (not prototype.constructor)
        if let Value::Object(ref mut obj) = (*obj.borrow_mut()).get_mut("prototype").unwrap() {
            obj.borrow_mut().insert("constructor".to_string(), v2);
        }
    }
    val
}

#[derive(Debug, Clone)]
pub struct ConstantTable {
    pub value: Vec<Value>,
    pub string: Vec<String>,
}

impl ConstantTable {
    pub fn new() -> ConstantTable {
        ConstantTable {
            value: vec![],
            string: vec![],
        }
    }
}

pub struct VM {
    pub jit: TracingJit,
    pub state: VMState,
    pub const_table: ConstantTable,
    pub insts: ByteCode,
    pub loop_bgn_end: HashMap<isize, isize>,
    pub op_table: [fn(&mut VM); 49],
    pub builtin_functions: Vec<unsafe fn(CallObject, Vec<Value>, &mut VM)>,
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub scope: Vec<CallObjectRef>,
    pub bp: usize,
    pub lp: usize,
    pub pc: isize,
    pub history: Vec<(usize, usize, usize, isize)>, // bp, lp, sp, return_pc
}

impl VM {
    pub fn new(global_vals: CallObjectRef) -> VM {
        global_vals.borrow_mut().set_value("console".to_string(), {
            let mut map = HashMap::new();
            map.insert(
                "log".to_string(),
                Value::BuiltinFunction(builtin::CONSOLE_LOG, CallObject::new(Value::Undefined)),
            );
            Value::Object(Rc::new(RefCell::new(map)))
        });

        global_vals.borrow_mut().set_value("process".to_string(), {
            let mut map = HashMap::new();
            map.insert("stdout".to_string(), {
                let mut map = HashMap::new();
                map.insert(
                    "write".to_string(),
                    Value::BuiltinFunction(
                        builtin::PROCESS_STDOUT_WRITE,
                        CallObject::new(Value::Undefined),
                    ),
                );
                Value::Object(Rc::new(RefCell::new(map)))
            });
            Value::Object(Rc::new(RefCell::new(map)))
        });

        #[rustfmt::skip]
        global_vals.borrow_mut().set_value("Math".to_string(), {
            let mut map = HashMap::new();
            map.insert("PI".to_string(), Value::Number(::std::f64::consts::PI));
            map.insert("floor".to_string(),
                Value::BuiltinFunction(builtin::MATH_FLOOR, CallObject::new(Value::Undefined)));
            map.insert("random".to_string(),
                Value::BuiltinFunction(builtin::MATH_RANDOM, CallObject::new(Value::Undefined)));
            map.insert("pow".to_string(),
                Value::BuiltinFunction(builtin::MATH_POW, CallObject::new(Value::Undefined)));
            map.insert("abs".to_string(),
                Value::BuiltinFunction(builtin::MATH_ABS, CallObject::new(Value::Undefined)));
            map.insert("acos".to_string(),
                Value::BuiltinFunction(builtin::MATH_ACOS, CallObject::new(Value::Undefined)));
            map.insert("acosh".to_string(),
                Value::BuiltinFunction(builtin::MATH_ACOSH, CallObject::new(Value::Undefined)));
            map.insert("asin".to_string(),
                Value::BuiltinFunction(builtin::MATH_ASIN, CallObject::new(Value::Undefined)));
            map.insert("asinh".to_string(),
                Value::BuiltinFunction(builtin::MATH_ASINH, CallObject::new(Value::Undefined)));
            map.insert("atan".to_string(),
                Value::BuiltinFunction(builtin::MATH_ATAN, CallObject::new(Value::Undefined)));
            map.insert("atanh".to_string(),
                Value::BuiltinFunction(builtin::MATH_ATANH, CallObject::new(Value::Undefined)));
            map.insert("atan2".to_string(),
                Value::BuiltinFunction(builtin::MATH_ATAN2, CallObject::new(Value::Undefined)));
            map.insert("cbrt".to_string(),
                Value::BuiltinFunction(builtin::MATH_CBRT, CallObject::new(Value::Undefined)));
            map.insert("ceil".to_string(),
                Value::BuiltinFunction(builtin::MATH_CEIL, CallObject::new(Value::Undefined)));
            map.insert("clz32".to_string(),
                Value::BuiltinFunction(builtin::MATH_CLZ32, CallObject::new(Value::Undefined)));
            map.insert("cos".to_string(),
                Value::BuiltinFunction(builtin::MATH_COS, CallObject::new(Value::Undefined)));
            map.insert("cosh".to_string(),
                Value::BuiltinFunction(builtin::MATH_COSH, CallObject::new(Value::Undefined)));
            map.insert("exp".to_string(),
                Value::BuiltinFunction(builtin::MATH_EXP, CallObject::new(Value::Undefined)));
            map.insert("expm1".to_string(),
                Value::BuiltinFunction(builtin::MATH_EXPM1, CallObject::new(Value::Undefined)));
            map.insert("fround".to_string(),
                Value::BuiltinFunction(builtin::MATH_FROUND, CallObject::new(Value::Undefined)));
            map.insert("hypot".to_string(),
                Value::BuiltinFunction(builtin::MATH_HYPOT, CallObject::new(Value::Undefined)));
            map.insert("log".to_string(),
                Value::BuiltinFunction(builtin::MATH_LOG, CallObject::new(Value::Undefined)));
            map.insert("log1p".to_string(),
                Value::BuiltinFunction(builtin::MATH_LOG1P, CallObject::new(Value::Undefined)));
            map.insert("log10".to_string(),
                Value::BuiltinFunction(builtin::MATH_LOG10, CallObject::new(Value::Undefined)));
            map.insert("log2".to_string(),
                Value::BuiltinFunction(builtin::MATH_LOG2, CallObject::new(Value::Undefined)));
            map.insert("max".to_string(),
                Value::BuiltinFunction(builtin::MATH_MAX, CallObject::new(Value::Undefined)));
            map.insert("min".to_string(),
                Value::BuiltinFunction(builtin::MATH_MIN, CallObject::new(Value::Undefined)));
            map.insert("round".to_string(),
                Value::BuiltinFunction(builtin::MATH_ROUND, CallObject::new(Value::Undefined)));
            map.insert("sign".to_string(),
                Value::BuiltinFunction(builtin::MATH_SIGN, CallObject::new(Value::Undefined)));
            map.insert("sin".to_string(),
                Value::BuiltinFunction(builtin::MATH_SIN, CallObject::new(Value::Undefined)));
            map.insert("sinh".to_string(),
                Value::BuiltinFunction(builtin::MATH_SINH, CallObject::new(Value::Undefined)));
            map.insert("sqrt".to_string(),
                Value::BuiltinFunction(builtin::MATH_SQRT, CallObject::new(Value::Undefined)));
            map.insert("tan".to_string(),
                Value::BuiltinFunction(builtin::MATH_TAN, CallObject::new(Value::Undefined)));
            map.insert("tanh".to_string(),
                Value::BuiltinFunction(builtin::MATH_TANH, CallObject::new(Value::Undefined)));
            map.insert("trunc".to_string(),
                Value::BuiltinFunction(builtin::MATH_TRUNC, CallObject::new(Value::Undefined)));
            Value::Object(Rc::new(RefCell::new(map)))
        });

        VM {
            jit: unsafe { TracingJit::new() },
            state: VMState {
                stack: { Vec::with_capacity(128) },
                scope: vec![global_vals],
                history: {
                    let mut s = Vec::with_capacity(128);
                    s.push((0, 0, 0, 0));
                    s
                },
                bp: 0,
                lp: 0,
                pc: 0isize,
            },
            const_table: ConstantTable::new(),
            insts: vec![],
            loop_bgn_end: HashMap::new(),
            op_table: [
                end,
                create_context,
                construct,
                create_object,
                create_array,
                push_int8,
                push_int32,
                push_false,
                push_true,
                push_const,
                push_this,
                push_arguments,
                neg,
                add,
                sub,
                mul,
                div,
                rem,
                lt,
                gt,
                le,
                ge,
                eq,
                ne,
                seq,
                sne,
                and,
                or,
                get_member,
                set_member,
                get_global,
                set_global,
                get_local,
                set_local,
                get_arg_local,
                set_arg_local,
                jmp_if_false,
                jmp,
                call,
                return_,
                assign_func_rest_param,
                double,
                pop,
                land,
                lor,
                set_cur_callobj,
                get_name,
                set_name,
                decl_var,
            ],
            builtin_functions: vec![
                builtin::console_log,
                builtin::process_stdout_write,
                builtin::array_push,
                builtin::math_floor,
                builtin::math_random,
                builtin::math_pow,
                builtin::math_abs,
                builtin::math_acos,
                builtin::math_acosh,
                builtin::math_asin,
                builtin::math_asinh,
                builtin::math_atan,
                builtin::math_atanh,
                builtin::math_atan2,
                builtin::math_cbrt,
                builtin::math_ceil,
                builtin::math_clz32,
                builtin::math_cos,
                builtin::math_cosh,
                builtin::math_exp,
                builtin::math_expm1,
                builtin::math_fround,
                builtin::math_hypot,
                builtin::math_log,
                builtin::math_log1p,
                builtin::math_log10,
                builtin::math_log2,
                builtin::math_max,
                builtin::math_min,
                builtin::math_round,
                builtin::math_sign,
                builtin::math_sin,
                builtin::math_sinh,
                builtin::math_sqrt,
                builtin::math_tan,
                builtin::math_tanh,
                builtin::math_trunc,
                builtin::function_prototype_call,
            ],
        }
    }
}

impl VM {
    pub fn run(&mut self, insts: ByteCode) {
        self.insts = insts;
        // Unlock the mutex and start the profiler
        // PROFILER
        //     .lock()
        //     .unwrap()
        //     .start("./my-prof.profile")
        //     .expect("Couldn't start");

        self.do_run();

        // Unwrap the mutex and stop the profiler
        // PROFILER.lock().unwrap().stop().expect("Couldn't stop");
    }

    pub fn do_run(&mut self) {
        loop {
            if let Some(end) = self.loop_bgn_end.get(&self.state.pc) {
                unsafe {
                    // println!("range: [{:x}, {:x})", self.state.pc, end);
                    if let Some(pc) = self.jit.can_loop_jit(
                        &self.insts,
                        &self.const_table,
                        &mut self.state,
                        *end as usize,
                    ) {
                        self.state.pc = pc;
                        continue;
                    }
                }
            }
            let code = self.insts[self.state.pc as usize];
            self.op_table[code as usize](self);
            if code == VMInst::RETURN || code == VMInst::END {
                break;
            }
            // println!("stack trace: {:?} - {}", self.stack, *pc);
        }
    }
}

macro_rules! get_int8 {
    ($self:ident, $var:ident, $ty:ty) => {
        let $var = $self.insts[$self.state.pc as usize] as $ty;
        $self.state.pc += 1;
    };
}

macro_rules! get_int32 {
    ($self:ident, $var:ident, $ty:ty) => {
        let $var = (($self.insts[$self.state.pc as usize + 3] as $ty) << 24)
            + (($self.insts[$self.state.pc as usize + 2] as $ty) << 16)
            + (($self.insts[$self.state.pc as usize + 1] as $ty) << 8)
            + ($self.insts[$self.state.pc as usize + 0] as $ty);
        $self.state.pc += 4;
    };
}

fn end(_self: &mut VM) {}

fn create_context(self_: &mut VM) {
    self_.state.pc += 5; // create_context
}

fn construct(self_: &mut VM) {
    self_.state.pc += 1; // construct
    get_int32!(self_, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    match callee {
        Value::Function(dst, obj, mut callobj) => {
            // insert new 'this'
            let new_this = {
                let mut map = HashMap::new();
                map.insert(
                    "__proto__".to_string(),
                    (*obj)
                        .borrow()
                        .get("prototype")
                        .unwrap_or(&Value::Undefined)
                        .clone(),
                );
                Rc::new(RefCell::new(map))
            };

            callobj.vals = Rc::new(RefCell::new(HashMap::new()));

            // similar code is used some times. should make it a function.
            let mut args = vec![];
            let mut rest_args = vec![];
            let mut rest_param_name = None;
            for _ in 0..argc {
                args.push(self_.state.stack.pop().unwrap());
            }
            for (i, arg) in args.iter().enumerate() {
                if let Some(name) = callobj.get_parameter_nth_name(i) {
                    // When rest parameter
                    if callobj.params[i].1 {
                        rest_param_name = Some(name);
                        rest_args.push(arg.clone());
                    } else {
                        callobj.set_value(name, arg.clone());
                    }
                } else {
                    rest_args.push(arg.clone());
                }
            }
            if let Some(rest_param_name) = rest_param_name {
                callobj.set_value(
                    rest_param_name,
                    Value::Array(Rc::new(RefCell::new(ArrayValue::new(rest_args)))),
                );
            } else {
                for arg in rest_args {
                    callobj.arg_rest_vals.push(arg.clone());
                }
            }

            *callobj.this = Value::Object(new_this.clone());
            self_.state.scope.push(Rc::new(RefCell::new(callobj)));
            self_
                .state
                .history
                .push((0, 0, self_.state.stack.len(), self_.state.pc));
            self_.state.pc = dst as isize;

            self_.do_run();

            self_.state.scope.pop();

            match self_.state.stack.last_mut().unwrap() {
                &mut Value::Object(_)
                | &mut Value::Array(_)
                | &mut Value::Function(_, _, _)
                | &mut Value::BuiltinFunction(_, _) => {}
                others => *others = Value::Object(new_this),
            };
        }
        c => {
            println!("Constract: err: {:?}, pc = {}", c, self_.state.pc);
        }
    }
}

fn create_object(self_: &mut VM) {
    self_.state.pc += 1; // create_context
    get_int32!(self_, len, usize);

    let mut map = HashMap::new();
    for _ in 0..len {
        let name = if let Value::String(name) = self_.state.stack.pop().unwrap() {
            name.into_string().unwrap()
        } else {
            panic!()
        };
        let val = self_.state.stack.pop().unwrap();
        map.insert(name, val.clone());
    }
    self_
        .state
        .stack
        .push(Value::Object(Rc::new(RefCell::new(map))));
}

fn create_array(self_: &mut VM) {
    self_.state.pc += 1; // create_context
    get_int32!(self_, len, usize);

    let mut arr = vec![];
    for _ in 0..len {
        let val = self_.state.stack.pop().unwrap();
        arr.push(val);
    }

    self_
        .state
        .stack
        .push(Value::Array(Rc::new(RefCell::new(ArrayValue::new(arr)))));
}

fn push_int8(self_: &mut VM) {
    self_.state.pc += 1; // push_int
    get_int8!(self_, n, i32);
    self_.state.stack.push(Value::Number(n as f64));
}

fn push_int32(self_: &mut VM) {
    self_.state.pc += 1; // push_int
    get_int32!(self_, n, i32);
    self_.state.stack.push(Value::Number(n as f64));
}

fn push_false(self_: &mut VM) {
    self_.state.pc += 1; // push_false
    self_.state.stack.push(Value::Bool(false));
}

fn push_true(self_: &mut VM) {
    self_.state.pc += 1; // push_true
    self_.state.stack.push(Value::Bool(true));
}

fn push_const(self_: &mut VM) {
    self_.state.pc += 1; // push_const
    get_int32!(self_, n, usize);
    self_.state.stack.push(self_.const_table.value[n].clone());
}

fn push_this(self_: &mut VM) {
    self_.state.pc += 1; // push_this
    let this = *self_.state.scope.last().unwrap().borrow().this.clone();
    self_.state.stack.push(this);
}

fn push_arguments(self_: &mut VM) {
    self_.state.pc += 1; // push_arguments
    self_.state.stack.push(Value::Arguments);
}

fn neg(self_: &mut VM) {
    self_.state.pc += 1; // neg
    let expr = self_.state.stack.last_mut().unwrap();
    match expr {
        &mut Value::Number(ref mut n) => *n = -*n,
        _ => unimplemented!(),
    }
}

macro_rules! bin_op {
    ($name:ident, $binop:ident) => {
        fn $name(self_: &mut VM) {
            self_.state.pc += 1; // $name
            binary(self_, &BinOp::$binop);
        }
    };
}

bin_op!(add, Add);
bin_op!(sub, Sub);
bin_op!(mul, Mul);
bin_op!(div, Div);
bin_op!(rem, Rem);
bin_op!(lt, Lt);
bin_op!(gt, Gt);
bin_op!(le, Le);
bin_op!(ge, Ge);
bin_op!(eq, Eq);
bin_op!(ne, Ne);
bin_op!(seq, SEq);
bin_op!(sne, SNe);
bin_op!(and, And);
bin_op!(or, Or);

#[rustfmt::skip]
#[inline]
fn binary(self_: &mut VM, op: &BinOp) {
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();

    match op {
        &BinOp::Add => add (self_, lhs, rhs),
        &BinOp::Sub => sub (self_, lhs, rhs),
        &BinOp::Mul => mul (self_, lhs, rhs),
        &BinOp::Div => div (self_, lhs, rhs),
        &BinOp::Rem => rem (self_, lhs, rhs),
        &BinOp::Lt  => lt  (self_, lhs, rhs),
        &BinOp::Gt  => gt  (self_, lhs, rhs),
        &BinOp::Le  => le  (self_, lhs, rhs),
        &BinOp::Ge  => ge  (self_, lhs, rhs),
        &BinOp::Eq  => eq  (self_, lhs, rhs),
        &BinOp::Ne  => ne  (self_, lhs, rhs),
        &BinOp::SEq => seq (self_, lhs, rhs),
        &BinOp::SNe => sne (self_, lhs, rhs),
        &BinOp::And => and (self_, lhs, rhs),
        &BinOp::Or  => or  (self_, lhs, rhs),
        _ => unimplemented!(),
    }

    fn add(self_: &mut VM, lhs: Value, rhs: Value) {
        self_.state.stack.push(match (lhs, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (Value::Bool(false), Value::Number(x)) | (Value::Number(x), Value::Bool(false)) => {
                Value::Number(x)
            }
            (Value::Bool(true), Value::Number(x)) | (Value::Number(x), Value::Bool(true)) => {
                Value::Number(x + 1.0)
            }
            (Value::String(l), Value::Number(r)) => {
                Value::String(CString::new(format!("{}{}", l.to_str().unwrap(), r)).unwrap())
            }
            (Value::Number(l), Value::String(r)) => {
                Value::String(CString::new(format!("{}{}", l, r.to_str().unwrap())).unwrap())
            }
            (Value::String(l), Value::Bool(r)) => {
                Value::String(CString::new(format!("{}{}", l.to_str().unwrap(), r)).unwrap())
            }
            (Value::Bool(l), Value::String(r)) => {
                Value::String(CString::new(format!("{}{}", l, r.to_str().unwrap())).unwrap())
            }
            _ => unimplemented!(),
        })
    };
    fn sub(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        _ => unimplemented!(),
    }) };
    fn mul(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
        (Value::String(l), Value::Number(r)) => 
            Value::String(CString::new(l.to_str().unwrap().repeat(r as usize)).unwrap()),
        _ => unimplemented!(),
    }) };
    fn div(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        _ => unimplemented!(),
    }) };
    fn rem(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number((l as i64 % r as i64) as f64),
        _ => unimplemented!(),
    }) };
    fn lt(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l < r),
        _ => unimplemented!(),
    }) };
    fn gt(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l > r),
        _ => unimplemented!(),
    }) };
    fn le(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l <= r),
        _ => unimplemented!(),
    }) };
    fn ge(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l >= r),
        _ => unimplemented!(),
    }) };
    // TODO: Need more precise implementation 
    fn eq(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l == r),
        _ => unimplemented!(),
    }) };
    fn ne(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l != r),
        _ => unimplemented!(),
    }) };
    fn seq(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l == r),
        _ => unimplemented!(),
    }) };
    fn sne(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l != r),
        _ => unimplemented!(),
    }) };
    fn and(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) & (r as i64)) as f64),
        _ => unimplemented!(),
    }) };
    fn or(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) | (r as i64)) as f64),
        _ => unimplemented!(),
    }) };
}

fn get_member(self_: &mut VM) {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    match parent.clone() {
        Value::String(s) => {
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => self_.state.stack.push(Value::String(
                    CString::new(
                        s.to_str()
                            .unwrap()
                            .chars()
                            .nth(n as usize)
                            .unwrap()
                            .to_string(),
                    ).unwrap(),
                )),
                Value::String(ref member) if member.to_str().unwrap() == "length" => {
                    self_.state.stack.push(Value::Number(
                        s.to_str()
                            .unwrap()
                            .chars()
                            .fold(0, |x, c| x + c.len_utf16()) as f64,
                    ));
                }
                // TODO: Support all features.
                _ => self_.state.stack.push(Value::Undefined),
            }
        }
        Value::Object(map) => {
            match obj_find_val(&map.borrow().clone(), member.to_string().as_str()) {
                Value::Function(pos, map2, mut callobj) => {
                    self_.state.stack.push(Value::Function(pos, map2, {
                        *callobj.this = parent;
                        callobj
                    }))
                }
                Value::BuiltinFunction(id, mut callobj) => {
                    self_.state.stack.push(Value::BuiltinFunction(id, {
                        *callobj.this = parent;
                        callobj
                    }))
                }
                val => self_.state.stack.push(val),
            }
        }
        Value::Function(_, map, _) => {
            match obj_find_val(&map.borrow().clone(), member.to_string().as_str()) {
                Value::Function(pos, map2, mut callobj) => {
                    self_.state.stack.push(Value::Function(pos, map2, {
                        *callobj.this = parent;
                        callobj
                    }))
                }
                Value::BuiltinFunction(id, mut callobj) => {
                    self_.state.stack.push(Value::BuiltinFunction(id, {
                        *callobj.this = parent;
                        callobj
                    }))
                }
                val => self_.state.stack.push(val),
            }
        }
        Value::Array(map) => {
            let mut map = map.borrow_mut();
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => {
                    let arr = &map.elems;
                    if n as usize >= map.length {
                        self_.state.stack.push(Value::Undefined);
                    } else {
                        self_.state.stack.push(arr[n as usize].clone())
                    }
                }
                Value::String(ref s) if s.to_str().unwrap() == "length" => {
                    self_.state.stack.push(Value::Number(map.length as f64));
                }
                _ => match obj_find_val(&map.obj, member.to_string().as_str()) {
                    Value::BuiltinFunction(id, mut callobj) => {
                        self_.state.stack.push(Value::BuiltinFunction(id, {
                            *callobj.this = parent;
                            callobj
                        }))
                    }
                    Value::Function(pos, map2, mut callobj) => {
                        self_.state.stack.push(Value::Function(pos, map2, {
                            *callobj.this = parent;
                            callobj
                        }))
                    }
                    val => self_.state.stack.push(val),
                },
            }
        }
        Value::Arguments => {
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => {
                    let val = self_
                        .state
                        .scope
                        .last()
                        .unwrap()
                        .borrow()
                        .get_arguments_nth_value(n as usize);
                    self_.state.stack.push(val);
                }
                Value::String(ref s) if s.to_str().unwrap() == "length" => {
                    let length = self_
                        .state
                        .scope
                        .last()
                        .unwrap()
                        .borrow()
                        .get_arguments_length();
                    self_.state.stack.push(Value::Number(length as f64));
                }
                _ => self_.state.stack.push(Value::Undefined),
            }
        }
        e => unreachable!("{:?}", e),
    }
}

pub fn obj_find_val(obj: &HashMap<String, Value>, key: &str) -> Value {
    match obj.get(key) {
        Some(addr) => addr.clone(),
        None => match obj.get("__proto__") {
            Some(Value::Object(obj)) => obj_find_val(&*(*obj).borrow(), key),
            _ => Value::Undefined,
        },
    }
}

fn set_member(self_: &mut VM) {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = self_.state.stack.pop().unwrap();
    match parent {
        Value::Object(map) | Value::Function(_, map, _) => {
            *map.borrow_mut()
                .entry(member.to_string())
                .or_insert_with(|| Value::Undefined) = val;
        }
        Value::Array(map) => {
            let mut map = map.borrow_mut();
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => {
                    if n as usize >= map.length as usize {
                        map.length = n as usize;
                        unsafe {
                            map.elems.set_len(n as usize);
                        };
                    }
                    map.elems[n as usize] = val;
                }
                Value::String(ref s) if s.to_str().unwrap() == "length" => match val {
                    Value::Number(n) if n - n.floor() == 0.0 => map.length = n as usize,
                    _ => {}
                },
                _ => {
                    *map.obj
                        .entry(member.to_string())
                        .or_insert_with(|| Value::Undefined) = val
                }
            }
        }
        Value::Arguments => {
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => {
                    self_
                        .state
                        .scope
                        .last()
                        .unwrap()
                        .borrow_mut()
                        .set_arguments_nth_value(n as usize, val);
                }
                // TODO: 'length'
                _ => {}
            }
        }
        e => unreachable!("{:?}", e),
    }
}

fn get_global(self_: &mut VM) {
    self_.state.pc += 1; // get_global
                         // get_int32!(self_, n, usize);
                         // let val = (*(*self_.global_objects)
                         //     .borrow()
                         //     .get(self_.const_table.string[n].as_str())
                         //     .unwrap())
                         //     .clone();
                         // self_.state.stack.push(val);
}

fn set_global(self_: &mut VM) {
    self_.state.pc += 1; // set_global
                         // get_int32!(self_, n, usize);
                         // *(*self_.global_objects)
                         //     .borrow_mut()
                         //     .entry(self_.const_table.string[n].clone())
                         //     .or_insert_with(|| Value::Undefined) = self_.state.stack.pop().unwrap();
}

fn get_local(self_: &mut VM) {
    self_.state.pc += 1; // get_local
                         // get_int32!(self_, n, usize);
                         // let val = self_.state.stack[self_.state.lp + n].clone();
                         // self_.state.stack.push(val);
}

fn set_local(self_: &mut VM) {
    self_.state.pc += 1; // set_local
                         // get_int32!(self_, n, usize);
                         // let val = self_.state.stack.pop().unwrap();
                         // self_.state.stack[self_.state.lp + n] = val;
}

fn get_arg_local(self_: &mut VM) {
    self_.state.pc += 1; // get_arg_local
                         // get_int32!(self_, n, usize);
                         // let val = self_.state.stack[self_.state.bp + n].clone();
                         // self_.state.stack.push(val);
}

fn set_arg_local(self_: &mut VM) {
    self_.state.pc += 1; // set_arg_local
                         // get_int32!(self_, n, usize);
                         // let val = self_.state.stack.pop().unwrap();
                         // self_.state.stack[self_.state.bp + n] = val;
}

fn jmp(self_: &mut VM) {
    self_.state.pc += 1; // jmp
    get_int32!(self_, dst, i32);
    if dst < 0 {
        self_
            .loop_bgn_end
            .insert(self_.state.pc + dst as isize, self_.state.pc);
    }
    self_.state.pc += dst as isize;
}

fn jmp_if_false(self_: &mut VM) {
    self_.state.pc += 1; // jmp_if_false
    get_int32!(self_, dst, i32);
    let cond = self_.state.stack.pop().unwrap();
    if let Value::Bool(false) = cond {
        self_.state.pc += dst as isize
    }
}

pub fn call_function(self_: &mut VM, dst: usize, args: Vec<Value>, mut callobj: CallObject) {
    let argc = args.len();
    let mut args_all_numbers = true;
    let mut rest_args = vec![];
    let mut rest_param_name = None;
    for (i, arg) in args.iter().enumerate() {
        if let Some(name) = callobj.get_parameter_nth_name(i) {
            // When rest parameter
            if callobj.params[i].1 {
                rest_param_name = Some(name);
                rest_args.push(arg.clone());
            } else {
                callobj.set_value(name, arg.clone());
            }
        } else {
            rest_args.push(arg.clone());
        }

        match &arg {
            &Value::Number(_) => {}
            _ => args_all_numbers = false,
        }
    }
    if let Some(rest_param_name) = rest_param_name {
        callobj.set_value(
            rest_param_name,
            Value::Array(Rc::new(RefCell::new(ArrayValue::new(rest_args)))),
        );
    } else {
        for arg in rest_args {
            callobj.arg_rest_vals.push(arg.clone());
        }
    }

    self_.state.scope.push(Rc::new(RefCell::new(callobj)));

    if args_all_numbers {
        let scope = self_.state.scope.last().unwrap().borrow().clone();
        if let Some(f) = unsafe {
            self_
                .jit
                .can_jit(&self_.insts, &scope, &self_.const_table, dst, argc)
        } {
            self_
                .state
                .stack
                .push(unsafe { self_.jit.run_llvm_func(dst, f, args) });
            self_.state.scope.pop();
            return;
        }
    }

    self_
        .state
        .history
        .push((0, 0, self_.state.stack.len(), self_.state.pc));
    self_.state.pc = dst as isize;

    self_.do_run();

    self_.state.scope.pop();

    self_
        .jit
        .register_return_type(dst, self_.state.stack.last().unwrap());
}

fn call(self_: &mut VM) {
    self_.state.pc += 1; // Call
    get_int32!(self_, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    match callee {
        Value::BuiltinFunction(x, callobj) => {
            let mut args = vec![];
            for _ in 0..argc {
                args.push(self_.state.stack.pop().unwrap());
            }
            unsafe { self_.builtin_functions[x](callobj, args, self_) };
        }
        Value::Function(dst, _, mut callobj) => {
            callobj.vals = Rc::new(RefCell::new(HashMap::new()));

            let mut args = vec![];
            for _ in 0..argc {
                args.push(self_.state.stack.pop().unwrap());
            }

            call_function(self_, dst, args, callobj);
        }
        c => {
            println!("Call: err: {:?}, pc = {}", c, self_.state.pc);
        }
    }
}

fn return_(self_: &mut VM) {
    let len = self_.state.stack.len();
    // println!("s: {:?}", self_.state.stack);
    if let Some((_, _, sp, return_pc)) = self_.state.history.pop() {
        self_.state.stack.drain(sp..len - 1);
        self_.state.pc = return_pc;
    } else {
        unreachable!()
    }
    // println!("a: {:?}", self_.state.stack);
}

fn assign_func_rest_param(self_: &mut VM) {
    self_.state.pc += 1; // assign_func_rest_param
    get_int32!(self_, num_func_param, usize);
    get_int32!(self_, dst_var_id, usize);
    let mut rest_params = vec![];
    for i in num_func_param..(self_.state.lp - self_.state.bp) {
        rest_params.push(self_.state.stack[self_.state.bp + i].clone());
    }
    self_.state.stack[self_.state.lp + dst_var_id] =
        Value::Array(Rc::new(RefCell::new(ArrayValue::new(rest_params))));
}

fn double(self_: &mut VM) {
    self_.state.pc += 1; // double
    let stack_top_val = self_.state.stack.last().unwrap().clone();
    self_.state.stack.push(stack_top_val);
}

fn pop(self_: &mut VM) {
    self_.state.pc += 1; // double
    self_.state.stack.pop();
}

// land & lor are for JIT compiler. They don't make sense in VM.

fn land(self_: &mut VM) {
    self_.state.pc += 1; // land
}

fn lor(self_: &mut VM) {
    self_.state.pc += 1; // lor
}

fn set_cur_callobj(self_: &mut VM) {
    self_.state.pc += 1;
    if let Some(Value::Function(_, _, ref mut callobj)) = self_.state.stack.last_mut() {
        callobj.parent = Some(self_.state.scope.last().unwrap().clone());
    }
}

fn get_name(self_: &mut VM) {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = &self_.const_table.string[name_id];
    let val = self_.state.scope.last().unwrap().borrow().get_value(name);
    self_.state.stack.push(val);
}

fn set_name(self_: &mut VM) {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let val = self_.state.stack.pop().unwrap();
    self_
        .state
        .scope
        .last()
        .unwrap()
        .borrow_mut()
        .set_value_if_exist(name, val);
}

fn decl_var(self_: &mut VM) {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let val = self_.state.stack.pop().unwrap();
    self_
        .state
        .scope
        .last()
        .unwrap()
        .borrow_mut()
        .set_value(name, val);
}

// #[rustfmt::skip]
// pub fn vm2_test() {
//     let mut vm2 = VM::new();
//     vm2.const_table.value.push(Value::Function(41, Rc::new(RefCell::new(HashMap::new()))));
//     vm2.const_table.value.push(Value::String("log".to_string()));
//     vm2.const_table.string.push("console".to_string());
//
//     // Loop for 100,000,000
//     // AllocLocalVar(1, 1)
//     // Push(Number(0.0))
//     // SetLocal(1)
//     // GetLocal(1)
//     // Push(Number(100000000.0))
//     // Lt
//     // JmpIfFalse(6)
//     // GetLocal(1)
//     // Push(Number(1.0))
//     // Add
//     // SetLocal(1)
//     // Jmp(-8)
//     // End
//     // vm2.run(vec![
//     //         CREATE_CONTEXT, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // CreateContext 1, 1
//     //         PUSH_INT32, 0x00, 0x00, 0x00, 0x00, // PushInt 0
//     //         SET_LOCAL, 0x01, 0x00, 0x00, 0x00, // SetLocal 1
//     //         GET_LOCAL, 0x01, 0x00, 0x00, 0x00, // GetLocal 1
//     //         PUSH_INT32, 0x00, 0xe1, 0xf5, 0x05, // PushInt 100,000,000
//     //         LT, // Lt
//     //         JMP_IF_FALSE, 0x15, 0x00, 0x00, 0x00, // JmpIfFalse 21
//     //         GET_LOCAL, 0x01, 0x00, 0x00, 0x00, // GetLocal 1
//     //         PUSH_INT32, 0x01, 0x00, 0x00, 0x00, // PushInt 1
//     //         ADD, // Add
//     //         SET_LOCAL, 0x01, 0x00, 0x00, 0x00, // SetLocal 1
//     //         JMP, 0xdb, 0xff, 0xff, 0xff, // Jmp -37
//     //         END, // End
//     // ]);
//
//     // Fibo 10
//     // AllocLocalVar(0, 1)
//     // Push(Number(10.0))
//     // Push(Function(5, RefCell { value: {} }))
//     // Call(1)
//     // End
//     // AllocLocalVar(0, 1)
//     // GetLocal(0)
//     // Push(Number(2.0))
//     // Lt
//     // JmpIfFalse(3)
//     // Push(Number(1.0))
//     // Return
//     // GetLocal(0)
//     // Push(Number(1.0))
//     // Sub
//     // Push(Function(5, RefCell { value: {} }))
//     // Call(1)
//     // GetLocal(0)
//     // Push(Number(2.0))
//     // Sub
//     // Push(Function(5, RefCell { value: {} }))
//     // Call(1)
//     // Add
//     // Return
//     vm2.run(vec![
//         CREATE_CONTEXT, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // CreateContext 1, 1
//         PUSH_INT32, 35,0,0,0, // PushInt 10
//         PUSH_CONST, 0x00, 0x00, 0x00, 0x00, // PushConst 0
//         CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
//         GET_GLOBAL, 0x00, 0x00, 0x00, 0x00, // GetGlobal 0 (console)
//         PUSH_CONST, 0x01, 0x00, 0x00, 0x00, // PushConst 1 (log)
//         GET_MEMBER, // GetMember
//         CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
//         END, // End
//         CREATE_CONTEXT, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // CreateContext 0, 1
//         GET_LOCAL, 0x00, 0x00, 0x00, 0x00, // GetLocal 0
//         PUSH_INT32, 0x02, 0,0,0,// PushInt 2
//         LT, // Lt
//         JMP_IF_FALSE, 6, 0x00, 0x00, 0x00, // JmpIfFalse 6
//         PUSH_INT32, 0x01,0,0,0, // PushInt 1
//         RETURN, // Return
//         GET_LOCAL, 0x00, 0x00, 0x00, 0x00, // GetLocal 0
//         PUSH_INT32, 0x01,0,0,0, // PushInt 1
//         SUB, // Sub
//         PUSH_CONST, 0x00, 0x00, 0x00, 0x00, // PushConst 0
//         CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
//         GET_LOCAL, 0x00, 0x00, 0x00, 0x00, // GetLocal 0
//         PUSH_INT32, 0x02, 0,0,0,// PushInt 2
//         SUB, // Sub
//         PUSH_CONST, 0x00, 0x00, 0x00, 0x00, // PushConst 0
//         CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
//         ADD, // Add
//         RETURN, // Return
//     ]);
// }
