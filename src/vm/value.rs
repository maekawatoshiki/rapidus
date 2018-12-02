#![macro_use]

use rustc_hash::FxHashMap;
use std::ffi::CString;

// use cpuprofiler::PROFILER;

use super::callobj::{CallObject, CallObjectRef};
use super::error::*;
use builtin;
use builtin::{BuiltinFuncInfo, BuiltinFuncTy, BuiltinJITFuncInfo};
use builtins::function;
use bytecode_gen::ByteCode;
use gc;
use id::Id;

pub type FuncId = Id;

pub type RawStringPtr = *mut libc::c_char;

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub val: ValueBase,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueBase {
    Empty,
    Null,
    Undefined,
    Bool(bool),
    Number(f64),
    String(CString), // TODO: Using CString is good for JIT. However, we need better one instead.
    Function(Box<(FuncId, ByteCode, *mut FxHashMap<String, Value>, CallObject)>),
    BuiltinFunction(Box<(BuiltinFuncInfo, *mut FxHashMap<String, Value>, CallObject)>), // id(==0:unknown)
    Object(*mut FxHashMap<String, Value>), // Object(FxHashMap<String, Value>),
    Array(*mut ArrayValue),
    Arguments, // TODO: Should have CallObject
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayValue {
    pub elems: Vec<Value>,
    pub length: usize,
    pub obj: FxHashMap<String, Value>,
}

macro_rules! make_object {
    ($($property_name:ident : $val:expr),*) => { {
        let mut map = FxHashMap::default();
        $(map.insert(stringify!($property_name).to_string(), $val);)*
        Value::object(gc::new(map))
    } };
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

    pub fn empty() -> Value {
        Value::new(ValueBase::Empty)
    }

    pub fn null() -> Value {
        Value::new(ValueBase::Null)
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

    pub fn function(id: FuncId, iseq: ByteCode, callobj: CallObject) -> Value {
        let val = Value::new(ValueBase::Function(Box::new((
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
                    Value::new(ValueBase::Function(Box::new((
                        0,
                        vec![],
                        function::FUNCTION_PROTOTYPE.with(|x| *x),
                        CallObject::new(Value::undefined()),
                    )))),
                );
                hm
            }),
            callobj,
        ))));

        // let v2 = val.clone();
        // if let ValueBase::Function(box (_, _, ref mut obj, _)) = &mut val.val {
        //     // TODO: Add constructor of this function itself (==Function). (not prototype.constructor)
        //     unsafe {
        //         if let ValueBase::Object(ref mut obj) = (**obj).get_mut("prototype").unwrap().val {
        //             (**obj).insert("constructor".to_string(), v2);
        //         }
        //     }
        // }

        val
    }

    pub fn builtin_function_with_jit(
        func: BuiltinFuncTy,
        builtin_jit_func_info: BuiltinJITFuncInfo,
        callobj: CallObject,
    ) -> Value {
        Value::builtin_function_with_obj_and_prototype(
            func,
            Some(builtin_jit_func_info),
            callobj,
            FxHashMap::default(),
            Value::new(ValueBase::Object(gc::new(FxHashMap::default()))),
        )
    }

    pub fn default_builtin_function(func: BuiltinFuncTy) -> Value {
        Value::builtin_function_with_obj_and_prototype(
            func,
            None,
            CallObject::new(Value::undefined()),
            FxHashMap::default(),
            Value::new(ValueBase::Object(gc::new(FxHashMap::default()))),
        )
    }

    pub fn builtin_function(func: BuiltinFuncTy, callobj: CallObject) -> Value {
        Value::builtin_function_with_obj_and_prototype(
            func,
            None,
            callobj,
            FxHashMap::default(),
            Value::new(ValueBase::Object(gc::new(FxHashMap::default()))),
        )
    }

    pub fn builtin_function_with_obj_and_prototype(
        func: BuiltinFuncTy,
        builtin_jit_func_info: Option<BuiltinJITFuncInfo>,
        callobj: CallObject,
        mut obj: FxHashMap<String, Value>,
        prototype: Value,
    ) -> Value {
        obj.insert("prototype".to_string(), prototype);
        obj.insert(
            "__proto__".to_string(),
            Value::new(ValueBase::Object(gc::new({
                let mut hm = FxHashMap::default();
                hm.insert(
                    "apply".to_string(),
                    Value::new(ValueBase::BuiltinFunction(Box::new((
                        BuiltinFuncInfo::new(builtin::function_prototype_apply, None),
                        ::std::ptr::null_mut(),
                        CallObject::new(Value::undefined()),
                    )))),
                );
                hm.insert(
                    "call".to_string(),
                    Value::new(ValueBase::BuiltinFunction(Box::new((
                        BuiltinFuncInfo::new(builtin::function_prototype_call, None),
                        ::std::ptr::null_mut(),
                        CallObject::new(Value::undefined()),
                    )))),
                );
                hm
            }))),
        );

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
            BuiltinFuncInfo::new(func, builtin_jit_func_info),
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
        let property_of_number = || -> Value {
            use builtins::number::NUMBER_PROTOTYPE;
            match obj_find_val(
                NUMBER_PROTOTYPE.with(|x| unsafe { &**x }),
                property.to_string().as_str(),
            ).val
            {
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
            let get_by_idx = |n: usize| -> Value {
                let arr = &ary.elems;

                if n >= ary.length {
                    return Value::undefined();
                }

                match arr[n] {
                    Value {
                        val: ValueBase::Empty,
                        ..
                    } => Value::undefined(),
                    ref other => other.clone(),
                }
            };

            match property {
                // Index
                ValueBase::Number(n) if is_integer(n) && n >= 0.0 => get_by_idx(n as usize),
                ValueBase::String(ref s) if s.to_str().unwrap() == "length" => {
                    Value::number(ary.length as f64)
                }
                ValueBase::String(ref s) => {
                    // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-array-exotic-objects
                    let num = property.to_uint32();
                    if Value::number(num).to_string() == s.to_str().unwrap() {
                        get_by_idx(num as usize)
                    } else {
                        property_of_simple(&ary.obj)
                    }
                }
                _ => property_of_simple(&ary.obj),
            }
        };

        let property_of_arguments = || -> Value {
            unsafe {
                match property {
                    // Index
                    ValueBase::Number(n) if is_integer(n) && n >= 0.0 => callobjref
                        .and_then(|co| Some((**co).get_arguments_nth_value(n as usize).unwrap()))
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
                ValueBase::Number(_) => property_of_number(),
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

    pub fn set_number_if_possible(&mut self, n: f64) {
        if let ValueBase::Number(ref mut n_) = self.val {
            *n_ = n;
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

    pub fn to_uint32(&self) -> f64 {
        let num = self.to_number();
        let p2_32 = 4294967296i64;

        if num.is_nan() || num == 0.0 || num.is_infinite() {
            return 0.0;
        }

        let int32bit = (if num < 0.0 {
            -num.abs().floor()
        } else {
            num.abs().floor()
        } as i64 % p2_32) as f64;

        if int32bit < 0.0 {
            p2_32 as f64 + int32bit
        } else {
            int32bit
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

impl ValueBase {
    pub fn type_equal(&self, other: &ValueBase) -> bool {
        match (self, other) {
            (&ValueBase::Empty, ValueBase::Empty)
            | (&ValueBase::Null, ValueBase::Null)
            | (&ValueBase::Undefined, ValueBase::Undefined)
            | (&ValueBase::Bool(_), ValueBase::Bool(_))
            | (&ValueBase::Number(_), ValueBase::Number(_))
            | (&ValueBase::String(_), ValueBase::String(_))
            | (&ValueBase::Object(_), ValueBase::Object(_))
            | (&ValueBase::Function(_), ValueBase::Function(_))
            | (&ValueBase::BuiltinFunction(_), ValueBase::BuiltinFunction(_))
            | (ValueBase::Array(_), ValueBase::Array(_))
            | (ValueBase::Arguments, ValueBase::Arguments) => true,
            _ => false,
        }
    }
    // https://tc39.github.io/ecma262/#sec-abstract-equality-comparison
    pub fn abstract_equal(self, other: ValueBase) -> Result<bool, RuntimeError> {
        if self.type_equal(&other) {
            return self.strict_equal(other);
        }

        match (&self, &other) {
            (&ValueBase::Number(l), &ValueBase::String(_)) => Ok(l == other.to_number()),
            (&ValueBase::String(_), &ValueBase::Number(r)) => Ok(self.to_number() == r),
            (&ValueBase::Bool(_), _) => {
                Ok(ValueBase::Number(self.to_number()).abstract_equal(other)?)
            }
            (_, &ValueBase::Bool(_)) => {
                Ok(ValueBase::Number(other.to_number()).abstract_equal(self)?)
            }
            // TODO: Implement the following cases:
            //  8. If Type(x) is either String, Number, or Symbol and Type(y) is Object,
            //      return the result of the comparison x == ToPrimitive(y).
            //  9. If Type(x) is Object and Type(y) is either String, Number, or Symbol,
            //      return the result of the comparison ToPrimitive(x) == y.
            _ => Ok(false),
        }
    }

    // https://tc39.github.io/ecma262/#sec-strict-equality-comparison
    pub fn strict_equal(self, other: ValueBase) -> Result<bool, RuntimeError> {
        match (self, other) {
            (ValueBase::Empty, ValueBase::Empty) => unreachable!(),
            (ValueBase::Null, ValueBase::Null) => Ok(true),
            (ValueBase::Undefined, ValueBase::Undefined) => Ok(true),
            (ValueBase::Bool(l), ValueBase::Bool(r)) => Ok(l == r),
            (ValueBase::Number(l), ValueBase::Number(r)) if l.is_nan() || r.is_nan() => Ok(false),
            (ValueBase::Number(l), ValueBase::Number(r)) => Ok(l == r),
            (ValueBase::String(l), ValueBase::String(r)) => Ok(l == r),
            (ValueBase::Object(l), ValueBase::Object(r)) => Ok(l == r),
            (ValueBase::Function(l), ValueBase::Function(r)) => Ok(l.as_ref() == r.as_ref()),
            (ValueBase::BuiltinFunction(l), ValueBase::BuiltinFunction(r)) => {
                Ok(l.as_ref() == r.as_ref())
            }
            (ValueBase::Array(l), ValueBase::Array(r)) => Ok(l == r),
            (ValueBase::Arguments, ValueBase::Arguments) => return Err(RuntimeError::Unimplemented),
            _ => Ok(false),
        }
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
                hm.insert("__proto__".to_string(), Value::array(Self::prototype()));
                hm
            },
        }
    }

    pub fn prototype() -> *mut ArrayValue {
        use builtins::array::ARRAY_PROTOTYPE;
        ARRAY_PROTOTYPE.with(|x| x.clone())
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

    pub fn push(&mut self, val: Value) {
        self.elems.push(val);
        self.length += 1;
    }
}

// Utils

#[inline]
fn is_integer(f: f64) -> bool {
    f - f.floor() == 0.0
}

pub fn obj_find_val(obj: &FxHashMap<String, Value>, key: &str) -> Value {
    match obj.get(key) {
        Some(addr) => addr.clone(),
        None => match obj.get("__proto__") {
            Some(val) => match val.val {
                ValueBase::Function(box (_, _, obj, _))
                | ValueBase::BuiltinFunction(box (_, obj, _))
                | ValueBase::Object(obj) => unsafe { obj_find_val(&*obj, key) },
                ValueBase::Array(aryval) => unsafe { obj_find_val(&(*aryval).obj, key) },
                _ => Value::undefined(),
            },
            _ => Value::undefined(),
        },
    }
}
