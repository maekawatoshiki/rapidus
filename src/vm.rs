use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use std::ffi::CString;

use ansi_term::Colour;
use libc;
// use cpuprofiler::PROFILER;

use builtin;
use bytecode_gen::{ByteCode, VMInst};
use gc;
use id::Id;
use jit::{TracingJit, UniquePosition};
use node::BinOp;

pub type RawStringPtr = *mut libc::c_char;

pub type CallObjectRef = *mut CallObject;

pub type FuncId = Id;

#[derive(Clone, Debug, PartialEq)]
pub struct CallObject {
    pub vals: *mut FxHashMap<String, Value>,
    pub params: Vec<(String, bool)>, // (name, rest param?)
    pub arg_rest_vals: Vec<Value>,
    pub this: Box<Value>,
    pub parent: Option<CallObjectRef>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayValue {
    pub elems: Vec<Value>,
    pub length: usize,
    pub obj: FxHashMap<String, Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub val: ValueBase,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueBase {
    Undefined,
    Bool(bool),
    Number(f64),
    String(CString),
    Function(Box<(FuncId, ByteCode, *mut FxHashMap<String, Value>, CallObject)>),
    BuiltinFunction(Box<(usize, *mut FxHashMap<String, Value>, CallObject)>), // id(==0:unknown)
    Object(*mut FxHashMap<String, Value>), // Object(FxHashMap<String, Value>),
    Array(*mut ArrayValue),
    Arguments,
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
    pub loop_bgn_end: FxHashMap<UniquePosition, isize>,
    pub cur_func_id: FuncId, // id == 0: main
    pub op_table: [fn(&mut VM, &ByteCode); 50],
    pub builtin_functions: Vec<unsafe fn(CallObject, Vec<Value>, &mut VM)>,
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub scope: Vec<CallObjectRef>,
    pub pc: isize,
    pub history: Vec<(usize, isize)>, // sp, return_pc
}

impl CallObject {
    pub fn new(this: Value) -> CallObject {
        CallObject {
            vals: gc::new(FxHashMap::default()),
            params: vec![],
            arg_rest_vals: vec![],
            this: Box::new(this),
            parent: None,
        }
    }

    pub fn new_global() -> CallObjectRef {
        let vals = gc::new(FxHashMap::default());
        let callobj = gc::new(CallObject {
            vals: vals.clone(),
            params: vec![],
            arg_rest_vals: vec![],
            this: Box::new(Value::new(ValueBase::Undefined)),
            parent: None,
        });
        unsafe {
            *(*callobj).this = Value::new(ValueBase::Object(vals));
        }
        callobj
    }

    pub fn set_value(&mut self, name: String, val: Value) {
        unsafe {
            (*self.vals).insert(name, val);
        }
    }

    pub fn set_value_if_exist(&mut self, name: String, val: Value) {
        unsafe {
            match (*self.vals).entry(name.clone()) {
                Entry::Occupied(ref mut v) => *v.get_mut() = val,
                Entry::Vacant(v) => {
                    match self.parent {
                        Some(ref parent) => return (**parent).set_value_if_exist(name, val),
                        None => v.insert(val),
                    };
                }
            }
        }
    }

    pub fn get_value(&self, name: &String) -> Value {
        unsafe {
            if let Some(val) = (*self.vals).get(name) {
                return val.clone();
            }
            match self.parent {
                Some(ref parent) => return (**parent).get_value(name),
                None => {
                    runtime_error(format!("reference error: '{}' is not defined", name).as_str())
                }
            }
        }
    }

    pub fn get_arguments_nth_value(&self, n: usize) -> Value {
        if n < self.params.len() {
            let param_name = &self.params[n].0;
            return self.get_value(param_name);
        }

        let n = n - self.params.len();
        if n >= self.arg_rest_vals.len() {
            return Value::new(ValueBase::Undefined);
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

impl ArrayValue {
    pub fn new(arr: Vec<Value>) -> ArrayValue {
        let len = arr.len();
        ArrayValue {
            elems: arr,
            length: len,
            obj: {
                let mut hm = FxHashMap::default();
                hm.insert(
                    "__proto__".to_string(),
                    Value::new(ValueBase::Object(gc::new({
                        let mut hm = FxHashMap::default();
                        hm.insert(
                            "push".to_string(),
                            Value::builtin_function(
                                builtin::ARRAY_PUSH,
                                CallObject::new(Value::new(ValueBase::Undefined)),
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
        self.elems[0..self.length]
            .iter()
            .fold("".to_string(), |acc, val| {
                acc + val.to_string().as_str() + ","
            })
            .trim_right_matches(",")
            .to_string()
    }
}

impl Value {
    pub fn new(val: ValueBase) -> Value {
        Value {
            val: val,
            writable: true,
            enumerable: true,
            configurable: true,
        }
    }

    pub fn to_string(&self) -> String {
        self.val.to_string()
    }

    pub fn undefined() -> Value {
        Value::new(ValueBase::Undefined)
    }

    pub fn bool(b: bool) -> Value {
        Value::new(ValueBase::Bool(b))
    }

    pub fn number(n: f64) -> Value {
        Value::new(ValueBase::Number(n))
    }

    pub fn string(s: CString) -> Value {
        Value::new(ValueBase::String(s))
    }

    pub fn function(
        id: FuncId,
        iseq: ByteCode,
        obj: *mut FxHashMap<String, Value>,
        callobj: CallObject,
    ) -> Value {
        Value::new(ValueBase::Function(Box::new((id, iseq, obj, callobj))))
    }

    pub fn builtin_function(pc: usize, callobj: CallObject) -> Value {
        let mut obj = {
            let mut hm = FxHashMap::default();
            hm.insert(
                "prototype".to_string(),
                Value::new(ValueBase::Object(gc::new(FxHashMap::default()))),
            );
            hm.insert(
                "__proto__".to_string(),
                Value::new(ValueBase::Object(gc::new({
                    let mut hm = FxHashMap::default();
                    hm.insert(
                        "apply".to_string(),
                        Value::new(ValueBase::BuiltinFunction(Box::new((
                            builtin::FUNCTION_PROTOTYPE_APPLY,
                            ::std::ptr::null_mut(),
                            CallObject::new(Value::undefined()),
                        )))),
                    );
                    hm.insert(
                        "call".to_string(),
                        Value::new(ValueBase::BuiltinFunction(Box::new((
                            builtin::FUNCTION_PROTOTYPE_CALL,
                            ::std::ptr::null_mut(),
                            CallObject::new(Value::undefined()),
                        )))),
                    );
                    hm
                }))),
            );
            hm
        };

        let obj_ = obj.clone();

        if let ValueBase::Object(ref mut obj2) = obj.get_mut("__proto__").unwrap().val {
            unsafe {
                for name in ["apply", "call"].iter() {
                    if let ValueBase::BuiltinFunction(box (_, ref mut obj3, _)) =
                        (**obj2).get_mut(*name).unwrap().val
                    {
                        *obj3 = gc::new(obj_.clone());
                    }
                }
            }
        }

        Value::new(ValueBase::BuiltinFunction(Box::new((
            pc,
            gc::new(obj),
            callobj,
        ))))
    }

    pub fn object(obj: *mut FxHashMap<String, Value>) -> Value {
        Value::new(ValueBase::Object(obj))
    }

    pub fn array(ary: *mut ArrayValue) -> Value {
        Value::new(ValueBase::Array(ary))
    }

    pub fn arguments() -> Value {
        Value::new(ValueBase::Arguments)
    }

    pub fn get_property(&self, property: ValueBase, callobjref: Option<&CallObjectRef>) -> Value {
        let property_of_simple = |obj: &FxHashMap<String, Value>| -> Value {
            match obj_find_val(obj, property.to_string().as_str()).val {
                ValueBase::Function(box (id, iseq, map2, mut callobj)) => {
                    Value::new(ValueBase::Function(Box::new((id, iseq, map2, {
                        *callobj.this = self.clone();
                        callobj
                    }))))
                }
                ValueBase::BuiltinFunction(box (id, obj, mut callobj)) => {
                    Value::new(ValueBase::BuiltinFunction(Box::new((id, obj, {
                        *callobj.this = self.clone();
                        callobj
                    }))))
                }
                val => Value::new(val),
            }
        };
        let property_of_string = |s: &CString| -> Value {
            match property {
                // Character at the index 'n'
                ValueBase::Number(n) if is_integer(n) => Value::string(
                    CString::new(
                        s.to_str()
                            .unwrap()
                            .chars()
                            .nth(n as usize)
                            .unwrap()
                            .to_string(),
                    ).unwrap(),
                ),
                // Length of string. TODO: Is this implementation correct?
                ValueBase::String(ref member) if member.to_str().unwrap() == "length" => {
                    Value::number(
                        s.to_str()
                            .unwrap()
                            .chars()
                            .fold(0, |x, c| x + c.len_utf16()) as f64,
                    )
                }
                // TODO: Support all features.
                _ => Value::undefined(),
            }
        };
        let property_of_object =
            |properties: &FxHashMap<String, Value>| -> Value { property_of_simple(properties) };
        let property_of_array = |ary: &ArrayValue| -> Value {
            match property {
                // Index
                ValueBase::Number(n) if is_integer(n) => {
                    let arr = &ary.elems;
                    if n as usize >= ary.length {
                        Value::undefined()
                    } else {
                        arr[n as usize].clone()
                    }
                }
                ValueBase::String(ref s) if s.to_str().unwrap() == "length" => {
                    Value::number(ary.length as f64)
                }
                _ => property_of_simple(&ary.obj),
            }
        };
        let property_of_arguments = || -> Value {
            unsafe {
                match property {
                    // Index
                    ValueBase::Number(n) if is_integer(n) => callobjref
                        .and_then(|co| Some((**co).get_arguments_nth_value(n as usize)))
                        .unwrap_or_else(|| Value::undefined()),
                    ValueBase::String(ref s) if s.to_str().unwrap() == "length" => {
                        let length = callobjref
                            .and_then(|co| Some((**co).get_arguments_length()))
                            .unwrap_or(0);
                        Value::number(length as f64)
                    }
                    _ => Value::undefined(),
                }
            }
        };

        unsafe {
            match self.val {
                ValueBase::String(ref s) => property_of_string(s),
                ValueBase::BuiltinFunction(box (_, ref obj, _))
                | ValueBase::Function(box (_, _, ref obj, _))
                | ValueBase::Object(ref obj) => property_of_object(&**obj),
                ValueBase::Array(ref ary) => property_of_array(&**ary),
                ValueBase::Arguments => property_of_arguments(),
                // TODO: Implement
                _ => Value::undefined(),
            }
        }
    }
}

impl ValueBase {
    pub fn to_string(&self) -> String {
        match self {
            ValueBase::Undefined => "undefined".to_string(),
            ValueBase::Bool(b) => {
                if *b {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            ValueBase::Number(n) => {
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
            ValueBase::String(s) => s.clone().into_string().unwrap(),
            ValueBase::Array(ary_val) => unsafe { (**ary_val).to_string() },
            ValueBase::Object(_) => "[object Object]".to_string(),
            e => unimplemented!("{:?}", e),
        }
    }

    // TODO: Need a correct implementation!
    pub fn to_number(&self) -> f64 {
        fn str_to_num(s: &str) -> f64 {
            let all_ws = s.chars().all(|c| c.is_whitespace());

            if all_ws {
                return 0.0;
            }

            match s.parse::<f64>() {
                Ok(n) => n,
                _ => ::std::f64::NAN,
            }
        }

        fn ary_to_num(ary: &ArrayValue) -> f64 {
            match ary.length {
                0 => 0.0,
                // TODO: FIX!!!
                1 => match ary.elems[0].val {
                    ValueBase::Bool(_) => ::std::f64::NAN,
                    ref otherwise => otherwise.to_number(),
                },
                _ => ::std::f64::NAN,
            }
        }

        unsafe {
            match self {
                ValueBase::Undefined => ::std::f64::NAN,
                ValueBase::Bool(false) => 0.0,
                ValueBase::Bool(true) => 1.0,
                ValueBase::Number(n) => *n,
                ValueBase::String(s) => str_to_num(s.to_str().unwrap()),
                ValueBase::Array(ary) => ary_to_num(&**ary),
                _ => ::std::f64::NAN,
            }
        }
    }

    // TODO: Need a correct implementation!
    pub fn to_boolean(&self) -> bool {
        match self {
            ValueBase::Undefined => false,
            ValueBase::Bool(b) => *b,
            ValueBase::Number(n) if *n == 0.0 || n.is_nan() => false,
            ValueBase::Number(_) => true,
            ValueBase::String(s) if s.to_str().unwrap().len() == 0 => false,
            ValueBase::String(_) => true,
            ValueBase::Array(_) => true,
            ValueBase::Object(_) => true,
            _ => false,
        }
    }
}

pub fn new_value_function(id: FuncId, iseq: ByteCode, callobj: CallObject) -> Value {
    let mut val = Value::new(ValueBase::Function(Box::new((
        id,
        iseq,
        gc::new({
            let mut hm = FxHashMap::default();
            hm.insert(
                "prototype".to_string(),
                Value::new(ValueBase::Object(gc::new(FxHashMap::default()))),
            );
            hm.insert(
                "__proto__".to_string(),
                Value::new(ValueBase::Object(gc::new({
                    let mut hm = FxHashMap::default();
                    hm.insert(
                        "apply".to_string(),
                        Value::builtin_function(
                            builtin::FUNCTION_PROTOTYPE_APPLY,
                            CallObject::new(Value::undefined()),
                        ),
                    );
                    hm.insert(
                        "call".to_string(),
                        Value::builtin_function(
                            builtin::FUNCTION_PROTOTYPE_CALL,
                            CallObject::new(Value::undefined()),
                        ),
                    );
                    hm
                }))),
            );
            hm
        }),
        callobj,
    ))));

    let v2 = val.clone();
    if let ValueBase::Function(box (_, _, ref mut obj, _)) = &mut val.val {
        // TODO: Add constructor of this function itself (==Function). (not prototype.constructor)
        unsafe {
            if let ValueBase::Object(ref mut obj) = (**obj).get_mut("prototype").unwrap().val {
                (**obj).insert("constructor".to_string(), v2);
            }
        }
    }
    val
}

pub fn obj_find_val(obj: &FxHashMap<String, Value>, key: &str) -> Value {
    match obj.get(key) {
        Some(addr) => addr.clone(),
        None => match obj.get("__proto__") {
            Some(Value {
                val: ValueBase::Object(obj),
                ..
            }) => unsafe { obj_find_val(&**obj, key) },
            _ => Value::undefined(),
        },
    }
}

#[inline]
fn is_integer(f: f64) -> bool {
    f - f.floor() == 0.0
}

fn runtime_error(msg: &str) -> ! {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
    panic!()
}

impl VM {
    pub fn new(global_vals: CallObjectRef) -> VM {
        // TODO: Support for 'require' is not enough.
        unsafe {
            (*global_vals).set_value(
                "require".to_string(),
                Value::builtin_function(builtin::REQUIRE, CallObject::new(Value::undefined())),
            );

            let module_exports = Value::object(gc::new(FxHashMap::default()));
            (*global_vals).set_value("module".to_string(), {
                let mut map = FxHashMap::default();
                map.insert("exports".to_string(), module_exports.clone());
                Value::object(gc::new(map))
            });
            (*global_vals).set_value("exports".to_string(), module_exports);
        }

        unsafe {
            (*global_vals).set_value("console".to_string(), {
                let mut map = FxHashMap::default();
                map.insert(
                    "log".to_string(),
                    Value::builtin_function(
                        builtin::CONSOLE_LOG,
                        CallObject::new(Value::undefined()),
                    ),
                );
                Value::object(gc::new(map))
            });
        }

        unsafe {
            (*global_vals).set_value("process".to_string(), {
                let mut map = FxHashMap::default();
                map.insert("stdout".to_string(), {
                    let mut map = FxHashMap::default();
                    map.insert(
                        "write".to_string(),
                        Value::builtin_function(
                            builtin::PROCESS_STDOUT_WRITE,
                            CallObject::new(Value::undefined()),
                        ),
                    );
                    Value::object(gc::new(map))
                });
                Value::object(gc::new(map))
            });
        }

        unsafe {
            (*global_vals).set_value("Math".to_string(), {
                let mut map = FxHashMap::default();
                map.insert("PI".to_string(), Value::number(::std::f64::consts::PI));
                map.insert(
                    "floor".to_string(),
                    Value::builtin_function(
                        builtin::MATH_FLOOR,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "random".to_string(),
                    Value::builtin_function(
                        builtin::MATH_RANDOM,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "pow".to_string(),
                    Value::builtin_function(builtin::MATH_POW, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "abs".to_string(),
                    Value::builtin_function(builtin::MATH_ABS, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "acos".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ACOS,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "acosh".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ACOSH,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "asin".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ASIN,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "asinh".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ASINH,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "atan".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ATAN,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "atanh".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ATANH,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "atan2".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ATAN2,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "cbrt".to_string(),
                    Value::builtin_function(
                        builtin::MATH_CBRT,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "ceil".to_string(),
                    Value::builtin_function(
                        builtin::MATH_CEIL,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "clz32".to_string(),
                    Value::builtin_function(
                        builtin::MATH_CLZ32,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "cos".to_string(),
                    Value::builtin_function(builtin::MATH_COS, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "cosh".to_string(),
                    Value::builtin_function(
                        builtin::MATH_COSH,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "exp".to_string(),
                    Value::builtin_function(builtin::MATH_EXP, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "expm1".to_string(),
                    Value::builtin_function(
                        builtin::MATH_EXPM1,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "fround".to_string(),
                    Value::builtin_function(
                        builtin::MATH_FROUND,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "hypot".to_string(),
                    Value::builtin_function(
                        builtin::MATH_HYPOT,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "log".to_string(),
                    Value::builtin_function(builtin::MATH_LOG, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "log1p".to_string(),
                    Value::builtin_function(
                        builtin::MATH_LOG1P,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "log10".to_string(),
                    Value::builtin_function(
                        builtin::MATH_LOG10,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "log2".to_string(),
                    Value::builtin_function(
                        builtin::MATH_LOG2,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "max".to_string(),
                    Value::builtin_function(builtin::MATH_MAX, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "min".to_string(),
                    Value::builtin_function(builtin::MATH_MIN, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "round".to_string(),
                    Value::builtin_function(
                        builtin::MATH_ROUND,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "sign".to_string(),
                    Value::builtin_function(
                        builtin::MATH_SIGN,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "sin".to_string(),
                    Value::builtin_function(builtin::MATH_SIN, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "sinh".to_string(),
                    Value::builtin_function(
                        builtin::MATH_SINH,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "sqrt".to_string(),
                    Value::builtin_function(
                        builtin::MATH_SQRT,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "tan".to_string(),
                    Value::builtin_function(builtin::MATH_TAN, CallObject::new(Value::undefined())),
                );
                map.insert(
                    "tanh".to_string(),
                    Value::builtin_function(
                        builtin::MATH_TANH,
                        CallObject::new(Value::undefined()),
                    ),
                );
                map.insert(
                    "trunc".to_string(),
                    Value::builtin_function(
                        builtin::MATH_TRUNC,
                        CallObject::new(Value::undefined()),
                    ),
                );
                Value::object(gc::new(map))
            });
        }

        VM {
            jit: unsafe { TracingJit::new() },
            state: VMState {
                stack: { Vec::with_capacity(128) },
                scope: vec![global_vals],
                history: {
                    let mut s = Vec::with_capacity(128);
                    s.push((0, 0));
                    s
                },
                pc: 0isize,
            },
            const_table: ConstantTable::new(),
            loop_bgn_end: FxHashMap::default(),
            cur_func_id: 0, // 0 is main
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
                push_undefined,
                lnot,
                posi,
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
                xor,
                shl,
                shr,
                zfshr,
                get_member,
                set_member,
                jmp_if_false,
                jmp,
                call,
                return_,
                double,
                pop,
                land,
                lor,
                set_cur_callobj,
                get_name,
                set_name,
                decl_var,
                cond_op,
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
                builtin::function_prototype_apply,
                builtin::function_prototype_call,
                builtin::require,
            ],
        }
    }
}

impl VM {
    pub fn run(&mut self, iseq: ByteCode) {
        // self.iseq = iseq;
        // Unlock the mutex and start the profiler
        // PROFILER
        //     .lock()
        //     .unwrap()
        //     .start("./my-prof.profile")
        //     .expect("Couldn't start");

        self.do_run(&iseq);

        // Unwrap the mutex and stop the profiler
        // PROFILER.lock().unwrap().stop().expect("Couldn't stop");
    }

    pub fn do_run(&mut self, iseq: &ByteCode) {
        let id = self.cur_func_id;
        loop {
            if let Some(end) = self
                .loop_bgn_end
                .get(&UniquePosition::new(id, self.state.pc as usize))
            {
                unsafe {
                    if let Some(pc) = self.jit.can_loop_jit(
                        id,
                        &iseq,
                        &self.const_table,
                        &mut self.state,
                        *end as usize,
                    ) {
                        self.state.pc = pc;
                        continue;
                    }
                }
            }
            let code = iseq[self.state.pc as usize];
            self.op_table[code as usize](self, iseq);
            if code == VMInst::RETURN || code == VMInst::END {
                break;
            }
            // println!("stack trace: {:?} - {}", self.stack, *pc);
        }
    }
}

macro_rules! get_int8 {
    ($self:ident, $iseq:ident, $var:ident, $ty:ty) => {
        let $var = $iseq[$self.state.pc as usize] as $ty;
        $self.state.pc += 1;
    };
}

macro_rules! get_int32 {
    ($self:ident, $iseq:ident, $var:ident, $ty:ty) => {
        let $var = (($iseq[$self.state.pc as usize + 3] as $ty) << 24)
            + (($iseq[$self.state.pc as usize + 2] as $ty) << 16)
            + (($iseq[$self.state.pc as usize + 1] as $ty) << 8)
            + ($iseq[$self.state.pc as usize + 0] as $ty);
        $self.state.pc += 4;
    };
}

fn end(_self: &mut VM, _iseq: &ByteCode) {}

fn create_context(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // create_context
}

fn construct(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // construct
    get_int32!(self_, iseq, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    match callee.val.clone() {
        ValueBase::Function(box (id, iseq, obj, mut callobj)) => {
            // insert new 'this'
            let new_this = {
                let mut map = FxHashMap::default();
                map.insert("__proto__".to_string(), unsafe {
                    (*obj)
                        .get("prototype")
                        .unwrap_or(&Value::undefined())
                        .clone()
                });
                gc::new(map)
            };

            callobj.vals = gc::new(FxHashMap::default());

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
                    Value::array(gc::new(ArrayValue::new(rest_args))),
                );
            } else {
                for arg in rest_args {
                    callobj.arg_rest_vals.push(arg.clone());
                }
            }

            *callobj.this = Value::object(new_this);
            self_.state.scope.push(gc::new(callobj));
            self_
                .state
                .history
                .push((self_.state.stack.len(), self_.state.pc));
            self_.state.pc = 0;
            let save_id = self_.cur_func_id;
            self_.cur_func_id = id;

            self_.do_run(&iseq);

            self_.cur_func_id = save_id;
            self_.state.scope.pop();

            match self_.state.stack.last_mut().unwrap() {
                &mut Value {
                    val: ValueBase::Object(_),
                    ..
                }
                | &mut Value {
                    val: ValueBase::Array(_),
                    ..
                }
                | &mut Value {
                    val: ValueBase::Function(box (_, _, _, _)),
                    ..
                }
                | &mut Value {
                    val: ValueBase::BuiltinFunction(box (_, _, _)),
                    ..
                } => {}
                others => *others = Value::object(new_this),
            };
        }
        c => {
            println!("Constract: err: {:?}, pc = {}", c, self_.state.pc);
        }
    }
}

fn create_object(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // create_object
    get_int32!(self_, iseq, len, usize);

    let mut map = FxHashMap::default();
    for _ in 0..len {
        let name = if let ValueBase::String(name) = self_.state.stack.pop().unwrap().val {
            name.into_string().unwrap()
        } else {
            unreachable!()
        };
        let val = self_.state.stack.pop().unwrap();
        map.insert(name, val.clone());
    }

    self_.state.stack.push(Value::object(gc::new(map)));

    gc::mark_and_sweep(&self_.state);
}

fn create_array(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // create_array
    get_int32!(self_, iseq, len, usize);

    let mut arr = vec![];
    for _ in 0..len {
        let val = self_.state.stack.pop().unwrap();
        arr.push(val);
    }

    self_
        .state
        .stack
        .push(Value::array(gc::new(ArrayValue::new(arr))));

    gc::mark_and_sweep(&self_.state);
}

fn push_int8(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // push_int
    get_int8!(self_, iseq, n, i8);
    self_.state.stack.push(Value::number(n as f64));
}

fn push_int32(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // push_int
    get_int32!(self_, iseq, n, i32);
    self_.state.stack.push(Value::number(n as f64));
}

fn push_false(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // push_false
    self_.state.stack.push(Value::bool(false));
}

fn push_true(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // push_true
    self_.state.stack.push(Value::bool(true));
}

fn push_const(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // push_const
    get_int32!(self_, iseq, n, usize);
    self_.state.stack.push(self_.const_table.value[n].clone());
}

fn push_this(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // push_this
    let this = unsafe { *(**self_.state.scope.last().unwrap()).this.clone() };
    self_.state.stack.push(this);
}

fn push_arguments(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // push_arguments
    self_.state.stack.push(Value::arguments());
}

fn push_undefined(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // push_defined
    self_.state.stack.push(Value::undefined());
}

fn lnot(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // lnot
    let expr = self_.state.stack.last_mut().unwrap();
    expr.val = ValueBase::Bool(!expr.val.to_boolean());
}

fn posi(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // posi
    let expr = self_.state.stack.last_mut().unwrap();
    expr.val = ValueBase::Number(expr.val.to_number());
}

fn neg(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // neg
    let expr = self_.state.stack.last_mut().unwrap();
    match &mut expr.val {
        &mut ValueBase::Number(ref mut n) => *n = -*n,
        _ => unimplemented!(),
    }
}

macro_rules! bin_op {
    ($name:ident, $binop:ident) => {
        fn $name(self_: &mut VM, _iseq: &ByteCode) {
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
bin_op!(xor, Xor);
bin_op!(shl, Shl);
bin_op!(shr, Shr);
bin_op!(zfshr, ZFShr);

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
        &BinOp::Xor => xor (self_, lhs, rhs),
        &BinOp::Shl => shl (self_, lhs, rhs),
        &BinOp::Shr => shr (self_, lhs, rhs),
        &BinOp::ZFShr => zfshr (self_, lhs, rhs),
        _ => unimplemented!(),
    }

    #[inline]
    fn add(self_: &mut VM, lhs: Value, rhs: Value) {
        self_.state.stack.push(match (lhs.val, rhs.val) {
            (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l + r),
            (ValueBase::Bool(false), ValueBase::Number(x)) | (ValueBase::Number(x), ValueBase::Bool(false)) => {
                Value::number(x)
            }
            (ValueBase::Bool(true), ValueBase::Number(x)) | (ValueBase::Number(x), ValueBase::Bool(true)) => {
                Value::number(x + 1.0)
            }
            (l, r) => {
                Value::string(CString::new(l.to_string() + r.to_string().as_str()).unwrap())
            }
        })
    };
    #[inline]
    fn sub(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l - r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn mul(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l * r),
        (ValueBase::String(l), ValueBase::Number(r)) => 
            Value::string(CString::new(l.to_str().unwrap().repeat(r as usize)).unwrap()),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn div(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l / r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn rem(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number((l as i64 % r as i64) as f64),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn lt(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l < r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l < r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn gt(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l > r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l > r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn le(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l <= r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l <= r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn ge(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l >= r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l >= r),
        _ => unimplemented!(),
    }) };
    #[inline]
    // TODO: Need more precise implementation 
    fn eq(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l == r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l == r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn ne(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l != r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l != r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn seq(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l == r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l == r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn sne(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l != r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l != r),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn and(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(((l as i64 as i32) & (r as i64 as i32)) as f64),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn or(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(((l as i64 as i32) | (r as i64 as i32)) as f64),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn xor(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(((l as i64 as i32) ^ (r as i64 as i32)) as f64),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn shl(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(((l as i64 as i32) << (r as i64 as i32)) as f64),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn shr(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(((l as i64 as i32) >> (r as i64 as i32)) as f64),
        _ => unimplemented!(),
    }) };
    #[inline]
    fn zfshr(self_: &mut VM, lhs: Value, rhs: Value) { self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(((l as u64 as u32) >> (r as u64 as u32)) as f64),
        _ => unimplemented!(),
    }) };
}

fn get_member(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = parent.get_property(member.val, Some(self_.state.scope.last().unwrap()));
    self_.state.stack.push(val);
}

fn set_member(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = self_.state.stack.pop().unwrap();
    // TODO: The following code should be a function (like Value::set_property).
    match parent.val {
        ValueBase::Object(map) | ValueBase::Function(box (_, _, map, _)) => unsafe {
            *(*map)
                .entry(member.to_string())
                .or_insert_with(|| Value::undefined()) = val;
        },
        ValueBase::Array(map) => unsafe {
            let mut map = &mut *map;
            match member.val {
                // Index
                ValueBase::Number(n) if n - n.floor() == 0.0 => {
                    if n as usize >= map.length as usize {
                        map.length = n as usize;
                        map.elems.set_len(n as usize);
                    }
                    map.elems[n as usize] = val;
                }
                ValueBase::String(ref s) if s.to_str().unwrap() == "length" => match val.val {
                    ValueBase::Number(n) if n - n.floor() == 0.0 => map.length = n as usize,
                    _ => {}
                },
                _ => {
                    *map.obj
                        .entry(member.to_string())
                        .or_insert_with(|| Value::undefined()) = val
                }
            }
        },
        ValueBase::Arguments => {
            match member.val {
                // Index
                ValueBase::Number(n) if n - n.floor() == 0.0 => unsafe {
                    (**self_.state.scope.last().unwrap()).set_arguments_nth_value(n as usize, val);
                },
                // TODO: 'length'
                _ => {}
            }
        }
        e => unreachable!("{:?}", e),
    }
}

fn jmp(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // jmp
    get_int32!(self_, iseq, dst, i32);
    if dst < 0 {
        self_.loop_bgn_end.insert(
            UniquePosition::new(self_.cur_func_id, (self_.state.pc + dst as isize) as usize),
            self_.state.pc,
        );
    }
    self_.state.pc += dst as isize;
}

fn jmp_if_false(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // jmp_if_false
    get_int32!(self_, iseq, dst, i32);
    let cond = self_.state.stack.pop().unwrap();
    if let ValueBase::Bool(false) = cond.val {
        self_.state.pc += dst as isize
    }
}

pub fn call_function(
    self_: &mut VM,
    id: FuncId,
    iseq: &ByteCode,
    args: Vec<Value>,
    mut callobj: CallObject,
) {
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

        match &arg.val {
            &ValueBase::Number(_) => {}
            _ => args_all_numbers = false,
        }
    }
    if let Some(rest_param_name) = rest_param_name {
        callobj.set_value(
            rest_param_name,
            Value::array(gc::new(ArrayValue::new(rest_args))),
        );
    } else {
        for arg in rest_args {
            callobj.arg_rest_vals.push(arg.clone());
        }
    }

    self_.state.scope.push(gc::new(callobj));

    if args_all_numbers {
        let scope = (*self_.state.scope.last().unwrap()).clone();
        if let Some(f) = unsafe {
            self_
                .jit
                .can_jit(id, iseq, &*scope, &self_.const_table, argc)
        } {
            self_
                .state
                .stack
                .push(unsafe { self_.jit.run_llvm_func(id, f, args) });
            self_.state.scope.pop();
            return;
        }
    }

    self_
        .state
        .history
        .push((self_.state.stack.len(), self_.state.pc));
    self_.state.pc = 0;

    let save_id = self_.cur_func_id;
    self_.cur_func_id = id;

    self_.do_run(iseq);

    self_.cur_func_id = save_id;
    self_.state.scope.pop();

    self_
        .jit
        .record_function_return_type(id, self_.state.stack.last().unwrap());
}

fn call(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1; // Call
    get_int32!(self_, iseq, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    match callee.val {
        ValueBase::BuiltinFunction(box (x, _, callobj)) => {
            let mut args = vec![];
            for _ in 0..argc {
                args.push(self_.state.stack.pop().unwrap());
            }
            unsafe { self_.builtin_functions[x](callobj, args, self_) };
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            callobj.vals = gc::new(FxHashMap::default());

            let mut args = vec![];
            for _ in 0..argc {
                args.push(self_.state.stack.pop().unwrap());
            }

            call_function(self_, id, iseq, args, callobj);
        }
        c => {
            runtime_error(
                format!(
                    "type error(pc:{}): '{:?}' is not a function but called",
                    self_.state.pc, c
                ).as_str(),
            );
        }
    }
}

fn return_(self_: &mut VM, _iseq: &ByteCode) {
    let len = self_.state.stack.len();
    if let Some((previous_sp, return_pc)) = self_.state.history.pop() {
        self_.state.stack.drain(previous_sp..len - 1);
        self_.state.pc = return_pc;
    } else {
        unreachable!()
    }
}

fn double(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // double
    let stack_top_val = self_.state.stack.last().unwrap().clone();
    self_.state.stack.push(stack_top_val);
}

fn pop(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // double
    self_.state.stack.pop();
}

// 'land' and 'lor' are for JIT compiler. Nope for VM.

fn land(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // land
}

fn lor(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1; // lor
}

fn set_cur_callobj(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1;
    if let Some(Value {
        val: ValueBase::Function(box (_, _, _, ref mut callobj)),
        ..
    }) = self_.state.stack.last_mut()
    {
        callobj.parent = Some(self_.state.scope.last().unwrap().clone());
    }
}

fn get_name(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = &self_.const_table.string[name_id];
    let val = unsafe { (**self_.state.scope.last().unwrap()).get_value(name) };
    self_.state.stack.push(val);
}

fn set_name(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let val = self_.state.stack.pop().unwrap();
    unsafe { (**self_.state.scope.last().unwrap()).set_value_if_exist(name, val) };
}

fn decl_var(self_: &mut VM, iseq: &ByteCode) {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let val = self_.state.stack.pop().unwrap();
    unsafe {
        (**self_.state.scope.last().unwrap()).set_value(name, val);
    }
}

// 'cond_op' is for JIT compiler. Nope for VM.
fn cond_op(self_: &mut VM, _iseq: &ByteCode) {
    self_.state.pc += 1;
}

// #[rustfmt::skip]
// pub fn vm2_test() {
//     let mut vm2 = VM::new();
//     vm2.const_table.value.push(Value::Function(41, Rc::new(RefCell::new(FxHashMap::default()))));
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
