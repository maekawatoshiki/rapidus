use std::boxed::Box;
use std::collections::HashMap;

use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::rc::Rc;

// use cpuprofiler::PROFILER;

use libc;

use bytecode_gen::ByteCode;
use jit::TracingJit;
use node::BinOp;
use vm_codegen::FunctionInfoForJIT;

pub type RawStringPtr = *mut libc::c_char;

pub unsafe fn alloc_rawstring(s: &str) -> RawStringPtr {
    let p = libc::calloc(1, s.len() + 2) as RawStringPtr;
    libc::strncpy(p, s.as_ptr() as *const i8, s.len());
    p
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Undefined,
    Bool(bool),
    Number(f64),
    String(RawStringPtr),
    Function(usize, Rc<RefCell<HashMap<String, Value>>>),
    NeedThis(Box<Value>),
    WithThis(Box<(Value, Value)>),               // Function, This
    EmbeddedFunction(usize), // unknown if usize == 0; specific function if usize > 0
    Object(Rc<RefCell<HashMap<String, Value>>>), // Object(HashMap<String, Value>),
    Array(Rc<RefCell<Vec<Value>>>, Rc<RefCell<HashMap<String, Value>>>), // Array, HashMap
}

impl Value {
    fn to_string(self) -> String {
        match self {
            Value::String(name) => unsafe { CStr::from_ptr(name).to_str().unwrap() }.to_string(),
            Value::Number(n) => format!("{}", n),
            _ => unimplemented!(),
        }
    }
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

pub const END: u8 = 0x00;
pub const CREATE_CONTEXT: u8 = 0x01;
pub const CONSTRACT: u8 = 0x02;
pub const CREATE_OBJECT: u8 = 0x03;
pub const CREATE_ARRAY: u8 = 0x04;
pub const PUSH_INT8: u8 = 0x05;
pub const PUSH_INT32: u8 = 0x06;
pub const PUSH_FALSE: u8 = 0x07;
pub const PUSH_TRUE: u8 = 0x08;
pub const PUSH_CONST: u8 = 0x09;
pub const PUSH_THIS: u8 = 0x0a;
pub const NEG: u8 = 0x0b;
pub const ADD: u8 = 0x0c;
pub const SUB: u8 = 0x0d;
pub const MUL: u8 = 0x0e;
pub const DIV: u8 = 0x0f;
pub const REM: u8 = 0x10;
pub const LT: u8 = 0x11;
pub const GT: u8 = 0x12;
pub const LE: u8 = 0x13;
pub const GE: u8 = 0x14;
pub const EQ: u8 = 0x15;
pub const NE: u8 = 0x16;
pub const GET_MEMBER: u8 = 0x17;
pub const SET_MEMBER: u8 = 0x18;
pub const GET_GLOBAL: u8 = 0x19;
pub const SET_GLOBAL: u8 = 0x1a;
pub const GET_LOCAL: u8 = 0x1b;
pub const SET_LOCAL: u8 = 0x1c;
pub const JMP_IF_FALSE: u8 = 0x1d;
pub const JMP: u8 = 0x1e;
pub const CALL: u8 = 0x1f;
pub const RETURN: u8 = 0x20;

pub struct VM {
    pub global_objects: Rc<RefCell<HashMap<String, Value>>>,
    pub jit: TracingJit,
    pub state: VMState,
    pub const_table: ConstantTable,
    pub insts: ByteCode,
    pub op_table: [fn(&mut VM); 33],
    pub builtin_functions: [unsafe fn(Vec<Value>); 2],
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub bp: usize,
    pub pc: isize,
    pub history: Vec<(usize, usize, isize)>, // bp, sp, return_pc
}

impl VM {
    pub fn new(func_addr_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>) -> VM {
        let mut obj = HashMap::new();

        obj.insert("console".to_string(), {
            let mut map = HashMap::new();
            map.insert("log".to_string(), Value::EmbeddedFunction(0));
            Value::Object(Rc::new(RefCell::new(map)))
        });

        obj.insert("process".to_string(), {
            let mut map = HashMap::new();
            map.insert("stdout".to_string(), {
                let mut map = HashMap::new();
                map.insert("write".to_string(), Value::EmbeddedFunction(1));
                Value::Object(Rc::new(RefCell::new(map)))
            });
            Value::Object(Rc::new(RefCell::new(map)))
        });

        let global_objects = Rc::new(RefCell::new(obj));

        VM {
            global_objects: global_objects.clone(),
            jit: unsafe { TracingJit::new(func_addr_in_bytecode_and_its_entity) },
            state: VMState {
                stack: {
                    let mut stack = Vec::with_capacity(128);
                    stack.push(Value::Object(global_objects.clone()));
                    stack
                },
                history: {
                    let mut s = Vec::with_capacity(128);
                    s.push((0, 0, 0));
                    s
                },
                bp: 0,
                pc: 0isize,
            },
            const_table: ConstantTable::new(),
            insts: vec![],
            op_table: [
                end,
                create_context,
                constract,
                create_object,
                create_array,
                push_int8,
                push_int32,
                push_false,
                push_true,
                push_const,
                push_this,
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
                get_member,
                set_member,
                get_global,
                set_global,
                get_local,
                set_local,
                jmp_if_false,
                jmp,
                call,
                return_,
            ],
            builtin_functions: [console_log, process_stdout_write],
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
            let code = self.insts[self.state.pc as usize];
            self.op_table[code as usize](self);
            if code == RETURN || code == END {
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
    self_.state.pc += 1; // create_context
    get_int32!(self_, n, usize);
    get_int32!(self_, argc, usize);

    let stack_len = self_.state.stack.len();
    if let Some((ref mut bp, ref mut sp, ref mut _return_pc)) = self_.state.history.last_mut() {
        *bp = self_.state.bp;
        *sp = stack_len - argc;
    } else {
        unreachable!()
    };
    self_.state.bp = stack_len - argc;

    // This code is slower -> self_.state.stack.resize(stack_len + n, Value::Undefined);
    for _ in 0..n {
        self_.state.stack.push(Value::Undefined);
    }
}

fn constract(self_: &mut VM) {
    self_.state.pc += 1; // constract
    get_int32!(self_, argc, usize);

    let mut callee = self_.state.stack.pop().unwrap();

    loop {
        match callee {
            Value::Function(dst, _) => {
                self_.state.history.push((0, 0, self_.state.pc));

                // insert new 'this'
                let pos = self_.state.stack.len() - argc;
                let new_this = Rc::new(RefCell::new(HashMap::new()));
                self_
                    .state
                    .stack
                    .insert(pos, Value::Object(new_this.clone()));

                self_.state.pc = dst as isize;
                self_.do_run();
                if self_.state.stack.last().unwrap() == &Value::Undefined {
                    self_.state.stack.pop();
                    self_.state.stack.push(Value::Object(new_this));
                }
                break;
            }
            Value::NeedThis(callee_) => {
                callee = *callee_;
            }
            Value::WithThis(box (callee_, _)) => {
                callee = callee_;
            }
            c => {
                println!("Constract: err: {:?}, pc = {}", c, self_.state.pc);
                break;
            }
        }
    }
}

fn create_object(self_: &mut VM) {
    self_.state.pc += 1; // create_context
    get_int32!(self_, len, usize);

    let mut map = HashMap::new();
    for _ in 0..len {
        let name = if let Value::String(name) = self_.state.stack.pop().unwrap() {
            unsafe { CStr::from_ptr(name).to_str().unwrap() }.to_string()
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

    self_.state.stack.push(Value::Array(
        Rc::new(RefCell::new(arr)),
        Rc::new(RefCell::new({
            let mut map = HashMap::new();
            map.insert("length".to_string(), Value::Number(len as f64));
            map
        })),
    ));
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
    let val = self_.state.stack[self_.state.bp].clone();
    self_.state.stack.push(val);
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

#[inline]
fn binary(self_: &mut VM, op: &BinOp) {
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    match (lhs, rhs) {
        (Value::Number(n1), Value::Number(n2)) => self_.state.stack.push(match op {
            &BinOp::Add => Value::Number(n1 + n2),
            &BinOp::Sub => Value::Number(n1 - n2),
            &BinOp::Mul => Value::Number(n1 * n2),
            &BinOp::Div => Value::Number(n1 / n2),
            &BinOp::Rem => Value::Number((n1 as i64 % n2 as i64) as f64),
            &BinOp::Lt => Value::Bool(n1 < n2),
            &BinOp::Gt => Value::Bool(n1 > n2),
            &BinOp::Le => Value::Bool(n1 <= n2),
            &BinOp::Ge => Value::Bool(n1 >= n2),
            &BinOp::Eq => Value::Bool(n1 == n2),
            &BinOp::Ne => Value::Bool(n1 != n2),
            _ => panic!(),
        }),
        (Value::String(s1), Value::Number(n2)) => self_.state.stack.push(match op {
            &BinOp::Add => {
                let concat = format!(
                    "{}{}",
                    unsafe { CStr::from_ptr(s1).to_str().unwrap() }.to_string(),
                    n2
                );
                unsafe { Value::String(alloc_rawstring(concat.as_str())) }
            }
            _ => panic!(),
        }),
        (Value::Number(n1), Value::String(s2)) => self_.state.stack.push(match op {
            &BinOp::Add => {
                let concat = format!(
                    "{}{}",
                    n1,
                    unsafe { CStr::from_ptr(s2).to_str().unwrap() }.to_string()
                );
                unsafe { Value::String(alloc_rawstring(concat.as_str())) }
            }
            _ => panic!(),
        }),
        (Value::String(s1), Value::String(s2)) => self_.state.stack.push(match op {
            &BinOp::Add => {
                let concat = format!(
                    "{}{}",
                    unsafe { CStr::from_ptr(s1).to_str().unwrap() }.to_string(),
                    unsafe { CStr::from_ptr(s2).to_str().unwrap() }.to_string()
                );
                unsafe { Value::String(alloc_rawstring(concat.as_str())) }
            }
            _ => panic!(),
        }),
        _ => {}
    }
}

fn get_member(self_: &mut VM) {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    match parent {
        Value::Object(map)
        | Value::Function(_, map)
        | Value::NeedThis(box Value::Function(_, map)) => {
            match map.borrow().get(member.to_string().as_str()) {
                Some(addr) => {
                    let val = addr.clone();
                    if let Value::NeedThis(callee) = val {
                        self_.state.stack.push(Value::WithThis(Box::new((
                            *callee,
                            Value::Object(map.clone()),
                        ))))
                    } else {
                        self_.state.stack.push(val)
                    }
                }
                None => self_.state.stack.push(Value::Undefined),
            }
        }
        Value::Array(arr, map) => {
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => {
                    let length =
                        if let Value::Number(length) = (*map).borrow().get("length").unwrap() {
                            *length as usize
                        } else {
                            unreachable!()
                        };
                    let arr = (*arr).borrow();
                    if n as usize >= length {
                        self_.state.stack.push(Value::Undefined);
                    } else {
                        self_.state.stack.push(arr[n as usize].clone())
                    }
                }
                _ => match map.borrow().get(member.to_string().as_str()) {
                    Some(addr) => {
                        let val = addr.clone();
                        if let Value::NeedThis(callee) = val {
                            self_.state.stack.push(Value::WithThis(Box::new((
                                *callee,
                                Value::Object(map.clone()),
                            ))))
                        } else {
                            self_.state.stack.push(val)
                        }
                    }
                    None => self_.state.stack.push(Value::Undefined),
                },
            }
        }
        _ => unreachable!(),
    }
}

fn set_member(self_: &mut VM) {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = self_.state.stack.pop().unwrap();
    match parent {
        Value::Object(map)
        | Value::Function(_, map)
        | Value::NeedThis(box Value::Function(_, map)) => {
            *map.borrow_mut()
                .entry(member.to_string())
                .or_insert_with(|| Value::Undefined) = val;
        }
        Value::Array(arr, map) => {
            match member {
                // Index
                Value::Number(n) if n - n.floor() == 0.0 => {
                    (*arr).borrow_mut()[n as usize] = val;
                }
                _ => {
                    *map.borrow_mut()
                        .entry(member.to_string())
                        .or_insert_with(|| Value::Undefined) = val
                }
            }
        }
        e => unreachable!("{:?}", e),
    }
}

fn get_global(self_: &mut VM) {
    self_.state.pc += 1; // get_global
    get_int32!(self_, n, usize);
    let val = (*(*self_.global_objects)
        .borrow()
        .get(self_.const_table.string[n].as_str())
        .unwrap())
        .clone();
    self_.state.stack.push(val);
}

fn set_global(self_: &mut VM) {
    self_.state.pc += 1; // set_global
    get_int32!(self_, n, usize);
    *(*self_.global_objects)
        .borrow_mut()
        .entry(self_.const_table.string[n].clone())
        .or_insert_with(|| Value::Undefined) = self_.state.stack.pop().unwrap();
}

fn get_local(self_: &mut VM) {
    self_.state.pc += 1; // get_local
    get_int32!(self_, n, usize);
    let val = self_.state.stack[self_.state.bp + n].clone();
    self_.state.stack.push(val);
}

fn set_local(self_: &mut VM) {
    self_.state.pc += 1; // set_local
    get_int32!(self_, n, usize);
    let val = self_.state.stack.pop().unwrap();
    self_.state.stack[self_.state.bp + n] = val;
}

fn jmp(self_: &mut VM) {
    self_.state.pc += 1; // jmp
    get_int32!(self_, dst, i32);
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

fn call(self_: &mut VM) {
    self_.state.pc += 1; // Call
    get_int32!(self_, argc, usize);

    let mut this = None;

    let mut callee = self_.state.stack.pop().unwrap();

    loop {
        match callee {
            Value::EmbeddedFunction(x) => {
                let mut args = vec![];
                for _ in 0..argc {
                    args.push(self_.state.stack.pop().unwrap());
                }
                args.reverse();
                unsafe { self_.builtin_functions[x](args) };
                break;
            }
            Value::Function(dst, _) => {
                if let Some(this) = this {
                    let pos = self_.state.stack.len() - argc;
                    self_.state.stack.insert(pos, this);
                }

                if args_all_number(&self_.state.stack, argc) {
                    unsafe {
                        if let Some(f) = self_.jit.can_jit(dst) {
                            let mut args = vec![];
                            for _ in 0..argc {
                                args.push(self_.state.stack.pop().unwrap());
                            }
                            args.reverse();
                            self_
                                .state
                                .stack
                                .push(self_.jit.run_llvm_func(dst, f, args));
                            break;
                        }
                    }
                }

                self_.state.history.push((0, 0, self_.state.pc));
                self_.state.pc = dst as isize;
                self_.do_run();
                self_
                    .jit
                    .register_return_type(dst, self_.state.stack.last().unwrap());
                break;
            }
            Value::NeedThis(callee_) => {
                this = Some(Value::Object(self_.global_objects.clone()));
                callee = *callee_;
            }
            Value::WithThis(box callee_this) => {
                this = Some(callee_this.1);
                callee = callee_this.0;
            }
            c => {
                println!("Call: err: {:?}, pc = {}", c, self_.state.pc);
                break;
            }
        }
    }

    fn args_all_number(stack: &Vec<Value>, argc: usize) -> bool {
        let stack_len = stack.len();
        stack[stack_len - argc..stack_len].iter().all(|v| match v {
            &Value::Number(_) => true,
            _ => false,
        })
    }
}

fn return_(self_: &mut VM) {
    let len = self_.state.stack.len();
    if let Some((bp, sp, return_pc)) = self_.state.history.pop() {
        self_.state.stack.drain(sp..len - 1);
        self_.state.pc = return_pc;
        self_.state.bp = bp;
    } else {
        unreachable!()
    }
}

// EmbeddedFunction(0)
unsafe fn console_log(args: Vec<Value>) {
    let args_len = args.len();
    for i in 0..args_len {
        match args[i] {
            Value::String(ref s) => {
                libc::printf(b"%s\0".as_ptr() as RawStringPtr, *s as RawStringPtr);
            }
            Value::Number(ref n) => {
                libc::printf(b"%.15g\0".as_ptr() as RawStringPtr, *n);
            }
            Value::Object(_) => debug_print(&args[i]),
            Value::Undefined => {
                libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
            }
            _ => {}
        }
        if args_len - 1 != i {
            libc::printf(b" \0".as_ptr() as RawStringPtr);
        }
    }
    libc::puts(b"\0".as_ptr() as RawStringPtr);
}

// EmbeddedFunction(1)
unsafe fn process_stdout_write(args: Vec<Value>) {
    let args_len = args.len();
    for i in 0..args_len {
        match args[i] {
            Value::String(ref s) => {
                libc::printf(b"%s\0".as_ptr() as RawStringPtr, *s as RawStringPtr);
            }
            Value::Number(ref n) => {
                libc::printf(b"%.15g\0".as_ptr() as RawStringPtr, *n);
            }
            Value::Undefined => {
                libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
            }
            _ => {}
        }
        if args_len - 1 != i {
            libc::printf(b" \0".as_ptr() as RawStringPtr);
        }
    }
}

unsafe fn debug_print(val: &Value) {
    match val {
        &Value::String(ref s) => {
            libc::printf("'%s'\0".as_ptr() as RawStringPtr, *s as RawStringPtr);
        }
        &Value::Number(ref n) => {
            libc::printf("%.15g\0".as_ptr() as RawStringPtr, *n);
        }
        &Value::Object(ref values) => {
            libc::printf("{ \0".as_ptr() as RawStringPtr);
            for (key, val) in &*(*values).borrow() {
                libc::printf(
                    "'%s'\0".as_ptr() as RawStringPtr,
                    CString::new(key.as_str()).unwrap().into_raw(),
                );
                libc::printf(": \0".as_ptr() as RawStringPtr);
                debug_print(&val);
                libc::printf(", \0".as_ptr() as RawStringPtr);
            }
            libc::printf("}\0".as_ptr() as RawStringPtr);
        }
        &Value::Undefined => {
            libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
        }
        _ => {}
    }
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
