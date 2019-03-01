#![macro_use]
use super::callobj::CallObject;
use super::error::*;
use builtin::{BuiltinFuncInfo, BuiltinFuncTy, BuiltinFuncTy2, BuiltinJITFuncInfo};
use builtins::function;
use bytecode_gen::ByteCode;
use chrono::{DateTime, Utc};
use gc;
use gc::GcType;
use id::{get_unique_id, Id};
use rand::random;
pub use rustc_hash::FxHashMap;
use std::ffi::CString;
use vm;

pub const UNINITIALIZED: u32 = 0;
pub const EMPTY: u32 = 1;
pub const NULL: u32 = 2;
pub const UNDEFINED: u32 = 3;

make_nanbox! {
    #[derive(Clone, PartialEq, Debug, Copy)]
    pub unsafe enum BoxedValue, Value2 {
        Number(f64),
        Bool(u8), // 0 | 1 = false | true
        String(*mut CString), // TODO: Using CString is good for JIT. However, we need better one instead.
        Object(*mut ObjectInfo), // Object(FxHashMap<String, Value>),
        Other(u32) // UNINITIALIZED | EMPTY | NULL | UNDEFINED
    }
}

#[derive(Clone, Debug)]
pub struct ObjectInfo {
    kind: ObjectKind2,
    property: FxHashMap<String, Property2>,
}

#[derive(Clone, Debug)]
pub enum ObjectKind2 {
    Function(FunctionObjectInfo),
    Normal,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Property2 {
    pub val: Value2,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone, Debug)]
pub struct FunctionObjectInfo {
    pub id: usize,
    pub name: Option<String>,
    pub kind: FunctionObjectKind,
}

#[derive(Clone)]
pub enum FunctionObjectKind {
    User {
        params: Vec<FunctionParameter>,
        var_names: Vec<String>,
        lex_names: Vec<String>,
        func_decls: Vec<Value2>,
        code: ByteCode,
    },
    Builtin(BuiltinFuncTy2),
}

#[derive(Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub is_rest_param: bool,
}

impl Value2 {
    pub fn undefined() -> Self {
        Value2::Other(UNDEFINED)
    }

    pub fn uninitialized() -> Self {
        Value2::Other(UNINITIALIZED)
    }

    pub fn builtin_function(
        memory_allocator: &mut gc::MemoryAllocator,
        name: String,
        func: BuiltinFuncTy2,
    ) -> Self {
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: random::<usize>(),
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            // TODO
            property: FxHashMap::default(),
        }))
    }

    pub fn function(
        memory_allocator: &mut gc::MemoryAllocator,
        name: Option<String>,
        params: Vec<FunctionParameter>,
        var_names: Vec<String>,
        lex_names: Vec<String>,
        func_decls: Vec<Value2>,
        code: ByteCode,
    ) -> Self {
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: random::<usize>(),
                name: name,
                kind: FunctionObjectKind::User {
                    params,
                    var_names,
                    lex_names,
                    func_decls,
                    code,
                },
            }),
            // TODO
            property: FxHashMap::default(),
        }))
    }

    pub fn as_function(&self) -> FunctionObjectInfo {
        match self {
            Value2::Object(obj) => {
                let obj = unsafe { &**obj };
                match obj.kind {
                    ObjectKind2::Function(ref info) => return info.clone(),
                    _ => panic!(),
                }
            }
            e => panic!("{:?}", e),
        }
    }
}

impl ::std::fmt::Debug for FunctionObjectKind {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FunctionObjectKind::User { .. } => "User",
                FunctionObjectKind::Builtin(_) => "Builtin",
            }
        )
    }
}

/////////////////////////////////////////////////////////

pub type FuncId = Id;

pub type RawStringPtr = *mut libc::c_char;

pub type NamePropPair = (String, Property);
/// 24 bytes
pub type PropMapRef = GcType<FxHashMap<String, Property>>;
pub type CallObjectRef = GcType<CallObject>;
pub type ArrayValueRef = GcType<ArrayValue>;

#[derive(Clone, PartialEq, Debug)]
pub struct Property {
    pub val: Value,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncInfo {
    pub func_id: FuncId,
    pub iseq: ByteCode,
    pub params: Vec<(String, bool)>, // (name, rest param?)
}

impl FuncInfo {
    pub fn new(id: FuncId, iseq: ByteCode, params: Vec<(String, bool)>) -> FuncInfo {
        FuncInfo {
            func_id: id,
            iseq: iseq,
            params: params,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ObjectKind {
    Function(Box<(FuncInfo, CallObjectRef)>),
    BuiltinFunction(Box<(BuiltinFuncInfo, CallObjectRef)>), // id(==0:unknown)
    Ordinary,
    Array(ArrayValueRef),
    Date(Box<(DateTime<Utc>)>),
    Arguments(vm::vm::VMState),
}

// 32 bytes
#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Uninitialized,
    Empty,
    Null,
    Undefined,
    Bool(bool),
    Number(f64),
    String(Box<CString>), // TODO: Using CString is good for JIT. However, we need better one instead.
    Object(PropMapRef, ObjectKind), // Object(FxHashMap<String, Value>),
}

#[derive(Clone, PartialEq)]
/// 32 bytes
pub struct ArrayValue {
    pub elems: Vec<Property>,
    pub length: usize,
}

#[macro_export]
macro_rules! make_npp {
    ($($property_name:ident : $val:expr),*) => {
        vec![ $((stringify!($property_name).to_string(), Property::new($val))),* ]
    };
}

#[macro_export]
macro_rules! make_object {
    ($($property_name:ident : $val:expr),*) => {
        Value::object_from_npp(&make_npp!($($property_name: $val),*))
    };
}

impl Property {
    pub fn new(val: Value) -> Property {
        Property {
            val: val,
            writable: true,
            enumerable: true,
            configurable: true,
        }
    }
}

impl Value {
    /// convert to Property.
    pub fn to_property(&self) -> Property {
        Property::new(self.clone())
    }

    pub fn empty() -> Value {
        Value::Empty
    }

    pub fn string(s: String) -> Value {
        Value::String(Box::new(CString::new(s).unwrap()))
    }

    /// generate JS function object.
    pub fn function(iseq: ByteCode, params: Vec<(String, bool)>, callobj: CallObjectRef) -> Value {
        let mut prototype = Value::object_from_npp(&vec![]);
        let kind = ObjectKind::Function(Box::new((
            FuncInfo::new(get_unique_id(), iseq, params),
            callobj.clone(),
        )));
        let val = Value::Object(
            Value::propmap_from_npp(&make_npp!(
                prototype:  prototype.clone(),
                __proto__:  function::FUNCTION_PROTOTYPE.with(|x| x.clone())
            )),
            kind.clone(),
        );

        prototype.set_constructor(val.clone());

        val
    }

    /// generate builtin function with JIT, blank PropMapRef and no prototype.
    pub fn builtin_function_with_jit(
        func: BuiltinFuncTy,
        builtin_jit_func_info: BuiltinJITFuncInfo,
    ) -> Value {
        Value::builtin_function(func, Some(builtin_jit_func_info), &mut vec![], None)
    }

    /// generate builtin function with blank PropMapRef and no prototype.
    pub fn default_builtin_function(func: BuiltinFuncTy) -> Value {
        Value::builtin_function(func, None, &mut vec![], None)
    }

    /// generate builtin function with JIT from NamePropPair and prototype.
    pub fn builtin_function(
        func: BuiltinFuncTy,
        builtin_jit_func_info: Option<BuiltinJITFuncInfo>,
        npp: &mut Vec<NamePropPair>,
        prototype: Option<Value>,
    ) -> Value {
        if let Some(prototype) = prototype {
            npp.push(("prototype".to_string(), Property::new(prototype)));
        }
        let map = Value::propmap_from_npp(npp);
        Value::Object(
            map,
            ObjectKind::BuiltinFunction(Box::new((
                BuiltinFuncInfo::new(func, builtin_jit_func_info),
                CallObject::new_with_this(Value::Undefined),
            ))),
        )
    }

    /// make new property map (PropMapRef) from npp.
    pub fn propmap_from_npp(npp: &Vec<NamePropPair>) -> PropMapRef {
        let mut map = FxHashMap::default();
        for p in npp {
            map.insert(p.0.clone(), p.1.clone());
        }
        gc::new(map)
    }

    /// register name-value pairs to property map.
    pub fn insert_propmap(mut map: PropMapRef, npp: &Vec<(&'static str, Value)>) {
        for p in npp {
            map.insert(p.0.to_string(), p.1.to_property());
        }
    }

    pub fn object(map: PropMapRef) -> Value {
        use builtins::object;
        let mut map = map.clone();
        map.entry("__proto__".to_string())
            .or_insert(object::OBJECT_PROTOTYPE.with(|x| x.clone()).to_property());
        Value::Object(map, ObjectKind::Ordinary)
    }

    /// make new object from npp.
    pub fn object_from_npp(npp: &Vec<NamePropPair>) -> Value {
        let map = Value::propmap_from_npp(&npp);
        Value::object(map)
    }

    pub fn array(map: PropMapRef) -> Value {
        let ary = ArrayValue::new(vec![]);
        Value::Object(map, ObjectKind::Array(gc::new(ary)))
    }

    /// make new array from elements.
    pub fn array_from_elems(elms: Vec<Value>) -> Value {
        let ary = ArrayValue::new(elms);
        Value::Object(
            {
                use builtins::array::ARRAY_PROTOTYPE;
                let npp = make_npp!(
                    __proto__:  ARRAY_PROTOTYPE.with(|x| x.clone())
                );
                Value::propmap_from_npp(&npp)
            },
            ObjectKind::Array(gc::new(ary)),
        )
    }

    pub fn date(time_val: DateTime<Utc>) -> Value {
        use builtins::date::DATE_PROTOTYPE;
        Value::Object(
            {
                let mut hm = FxHashMap::default();
                hm.insert(
                    "__proto__".to_string(),
                    Property::new(DATE_PROTOTYPE.with(|x| x.clone())),
                );
                gc::new(hm)
            },
            ObjectKind::Date(Box::new(time_val)),
        )
    }

    pub fn arguments(state: vm::vm::VMState) -> Value {
        Value::Object(
            Value::propmap_from_npp(&vec![]),
            ObjectKind::Arguments(state),
        )
    }

    pub fn get_property(&self, property: Value, callobjref: Option<CallObjectRef>) -> Value {
        let property_of_number = || -> Value {
            use builtins::number::NUMBER_PROTOTYPE;
            let val = NUMBER_PROTOTYPE.with(|x| x.clone());
            set_this(obj_find_val(val, property.to_string().as_str()), self)
        };

        let property_of_object = |obj: Value| -> Value {
            set_this(obj_find_val(obj, property.to_string().as_str()), self)
        };

        let property_of_string = |s: &CString| -> Value {
            match property {
                // Character at the index 'n'
                Value::Number(n) if is_integer(n) => Value::string(
                    s.to_str()
                        .unwrap()
                        .chars()
                        .nth(n as usize)
                        .unwrap()
                        .to_string(),
                ),
                // Length of string. TODO: Is this implementation correct?
                Value::String(ref member) if member.to_str().unwrap() == "length" => Value::Number(
                    s.to_str()
                        .unwrap()
                        .chars()
                        .fold(0, |x, c| x + c.len_utf16()) as f64,
                ),
                // TODO: Support all features.
                _ => Value::Undefined,
            }
        };

        let property_of_array = |obj: &Value| -> Value {
            let get_by_idx = |n: usize| -> Value {
                if let Value::Object(_, ObjectKind::Array(ref arrval)) = obj {
                    let arr = &(*arrval).elems;
                    if n >= (*arrval).length {
                        return Value::Undefined;
                    }

                    match arr[n].val {
                        Value::Empty => Value::Undefined,
                        ref other => other.clone(),
                    }
                } else {
                    unreachable!("get_property(): Value is not an array.");
                }
            };

            match property {
                // Index
                Value::Number(n) if is_integer(n) && n >= 0.0 => get_by_idx(n as usize),
                Value::String(ref s) if s.to_str().unwrap() == "length" => {
                    if let Value::Object(_, ObjectKind::Array(ref arrval)) = obj {
                        Value::Number((*arrval).length as f64)
                    } else {
                        unreachable!("get_property(): Value is not an array.");
                    }
                }
                Value::String(ref s) => {
                    // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-array-exotic-objects
                    let num = property.to_uint32();
                    if Value::Number(num).to_string() == s.to_str().unwrap() {
                        get_by_idx(num as usize)
                    } else {
                        set_this(obj_find_val(obj.clone(), &property.to_string()), self)
                    }
                }
                _ => obj_find_val(obj.clone(), &property.to_string()),
            }
        };

        let property_of_arguments = |state: vm::vm::VMState| -> Value {
            {
                match property {
                    // Index
                    Value::Number(n) if is_integer(n) && n >= 0.0 => callobjref
                        .and_then(|co| {
                            let _co = &*co;
                            Some(state.get_arguments_nth_value(n as usize).unwrap())
                        })
                        .unwrap_or_else(|| Value::Undefined),
                    Value::String(ref s) if s.to_str().unwrap() == "length" => {
                        let length = state.get_arguments_length();
                        Value::Number(length as f64)
                    }
                    _ => Value::Undefined,
                }
            }
        };

        match self {
            Value::Number(_) => property_of_number(),
            Value::String(ref s) => property_of_string(s),
            Value::Object(_, ObjectKind::Array(_)) => property_of_array(&*self),
            Value::Object(_, ObjectKind::Arguments(state)) => property_of_arguments(state.clone()),
            Value::Object(_, _) => property_of_object(self.clone()),
            _ => Value::Undefined,
        }
    }

    pub fn set_property(
        &mut self,
        property: Value,
        value: Value,
        _callobj: Option<CallObjectRef>,
    ) -> Result<(), RuntimeError> {
        fn set_by_idx(ary: &mut ArrayValue, n: usize, val: Value) {
            if n >= ary.length as usize {
                ary.length = n + 1;
                while ary.elems.len() < n + 1 {
                    ary.elems.push(Value::empty().to_property());
                }
            }
            ary.elems[n] = val.to_property();
        };

        match self {
            Value::Object(map, ObjectKind::Array(ref mut aryval)) => {
                match property {
                    // Index
                    Value::Number(n) if is_integer(n) && n >= 0.0 => {
                        set_by_idx(&mut *aryval, n as usize, value)
                    }
                    Value::String(ref s) if s.to_str().unwrap() == "length" => match value {
                        Value::Number(n) if is_integer(n) && n >= 0.0 => {
                            (*aryval).length = n as usize;
                            while (*aryval).elems.len() < n as usize + 1 {
                                (*aryval).elems.push(Value::empty().to_property());
                            }
                        }
                        _ => {}
                    },
                    // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-array-exotic-objects
                    Value::String(ref s)
                        if Value::Number(property.to_uint32()).to_string()
                            == s.to_str().unwrap() =>
                    {
                        let num = property.to_uint32();
                        set_by_idx(&mut *aryval, num as usize, value)
                    }
                    _ => {
                        let refval = (*map)
                            .entry(property.to_string())
                            .or_insert_with(|| Value::Undefined.to_property());
                        *refval = value.to_property();
                    }
                }
            }
            Value::Object(_, ObjectKind::Arguments(ref mut state)) => {
                match property {
                    // Index
                    Value::Number(n) if n - n.floor() == 0.0 => {
                        state.set_arguments_nth_value(n as usize, value)?;
                    }
                    // TODO: 'length'
                    _ => {}
                }
            }
            Value::Object(map, _) => {
                let refval = (*map)
                    .entry(property.to_string())
                    .or_insert_with(|| Value::Undefined.to_property());
                *refval = value.to_property();
            }
            _ => {}
        };
        Ok(())
    }

    pub fn set_number_if_possible(&mut self, n: f64) {
        if let Value::Number(ref mut n_) = self {
            *n_ = n;
        }
    }

    pub fn set_constructor(&mut self, constructor: Value) {
        match self {
            Value::Object(map, _) => {
                (*map).insert("constructor".to_string(), constructor.to_property());
            }
            _ => {}
        }
    }

    pub fn set_property_with_name(&mut self, name: String, val: Value) {
        match self {
            Value::Object(map, _) => {
                (*map).insert(name, val.to_property());
            }
            _ => {}
        }
    }
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Undefined => "undefined".to_string(),
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
            Value::Object(_, ObjectKind::Array(ary_val)) => (*ary_val).to_string(),
            Value::Object(_, ObjectKind::Ordinary) => "[object Object]".to_string(),
            Value::Object(_, ObjectKind::Date(box time_val)) => time_val.to_rfc3339(),
            Value::Object(_, ObjectKind::Function(_)) => "[Function]".to_string(),
            Value::Object(_, ObjectKind::BuiltinFunction(_)) => "[BuiltinFunc]".to_string(),
            Value::Null => "null".to_string(),
            Value::Empty => "empty".to_string(),
            _ => "NOT IMPLEMENTED".to_string(),
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
                    Value::Bool(_) => ::std::f64::NAN,
                    ref otherwise => otherwise.to_number(),
                },
                _ => ::std::f64::NAN,
            }
        }

        match self {
            Value::Undefined => ::std::f64::NAN,
            Value::Bool(false) => 0.0,
            Value::Bool(true) => 1.0,
            Value::Number(n) => *n,
            Value::String(s) => str_to_num(s.to_str().unwrap()),
            Value::Object(_, ObjectKind::Array(ary)) => ary_to_num(&*ary),
            _ => ::std::f64::NAN,
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
        } as i64
            % p2_32) as f64;

        if int32bit < 0.0 {
            p2_32 as f64 + int32bit
        } else {
            int32bit
        }
    }

    // TODO: Need a correct implementation!
    pub fn to_boolean(&self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Bool(b) => *b,
            Value::Number(n) if *n == 0.0 || n.is_nan() => false,
            Value::Number(_) => true,
            Value::String(s) if s.to_str().unwrap().len() == 0 => false,
            Value::String(_) => true,
            Value::Object(_, _) => true,
            _ => false,
        }
    }

    /// generate pretty print string for the object.
    /// - max_depth: tracing depth in an object's property.
    /// - indent: if true, output with indentation.
    pub fn format(&self, max_depth: usize, indent: bool) -> String {
        return self.format_(max_depth, max_depth, indent);
    }

    fn format_(&self, max_depth: usize, depth: usize, indent: bool) -> String {
        match self {
            Value::Undefined | Value::Bool(_) | Value::Number(_) | Value::Null | Value::Empty => {
                self.to_string()
            }
            Value::Uninitialized => "uninitialized".to_string(),
            Value::String(_) => format!("'{}'", self.to_string()),
            Value::Object(_, ObjectKind::Array(aryval)) => match depth {
                0 => "[Array]".to_string(),
                depth => {
                    //let aryval = &*aryval;
                    let str = aryval.elems[0..aryval.length]
                        .iter()
                        .fold("".to_string(), |acc, prop| {
                            acc + prop.val.format_(max_depth, depth - 1, indent).as_str() + ","
                        })
                        .trim_end_matches(",")
                        .to_string();
                    format!("[{}]", str)
                }
            },
            Value::Object(map, ObjectKind::Ordinary) => match depth {
                0 => "[Object]".to_string(),
                depth => {
                    let cr = |i: usize| {
                        if indent {
                            format!("\n{}", "  ".repeat(max_depth - depth + i))
                        } else {
                            "".to_string()
                        }
                    };
                    //let map = unsafe { &*map };
                    let str = map
                        .iter()
                        .fold("".to_string(), |acc, nvp| {
                            if nvp.0.as_str() == "__proto__" {
                                acc
                            } else {
                                format!(
                                    "{}{}{}:{},",
                                    acc,
                                    cr(1),
                                    nvp.0,
                                    nvp.1.val.format_(max_depth, depth - 1, indent)
                                )
                            }
                        })
                        .trim_end_matches(",")
                        .to_string();
                    format!("{{{}{}}}", str, cr(0))
                }
            },
            Value::Object(_, ObjectKind::Date(_)) => "[Date]".to_string(),
            Value::Object(_, ObjectKind::Function(_)) => "[Function]".to_string(),
            Value::Object(_, ObjectKind::BuiltinFunction(_)) => "[BuiltinFunc]".to_string(),
            Value::Object(_, ObjectKind::Arguments(_)) => "arguments".to_string(),
        }
    }

    pub fn kind(&self) -> String {
        match self {
            Value::Undefined | Value::Bool(_) | Value::Null | Value::Empty => self.to_string(),
            Value::Uninitialized => "uninitialized".to_string(),
            Value::Number(_) => "[Number]".to_string(),
            Value::String(_) => "[String]".to_string(),
            Value::Object(_, ObjectKind::Array(_)) => "[Array]".to_string(),
            Value::Object(_, ObjectKind::Date(_)) => "[Date]".to_string(),
            Value::Object(_, ObjectKind::Function(_)) => "[Function]".to_string(),
            Value::Object(_, ObjectKind::BuiltinFunction(_)) => "[BuiltinFunc]".to_string(),
            Value::Object(_, ObjectKind::Arguments(_)) => "[Arguments]".to_string(),
            Value::Object(_, _) => "[Object]".to_string(),
        }
    }
}

impl Value {
    pub fn type_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Empty, Value::Empty)
            | (Value::Null, Value::Null)
            | (Value::Undefined, Value::Undefined)
            | (Value::Bool(_), Value::Bool(_))
            | (Value::Number(_), Value::Number(_))
            | (Value::String(_), Value::String(_))
            | (Value::Object(_, ObjectKind::Ordinary), Value::Object(_, ObjectKind::Ordinary))
            | (
                Value::Object(_, ObjectKind::Function(_)),
                Value::Object(_, ObjectKind::Function(_)),
            )
            | (
                Value::Object(_, ObjectKind::BuiltinFunction(_)),
                Value::Object(_, ObjectKind::BuiltinFunction(_)),
            )
            | (Value::Object(_, ObjectKind::Array(_)), Value::Object(_, ObjectKind::Array(_)))
            | (
                Value::Object(_, ObjectKind::Arguments(_)),
                Value::Object(_, ObjectKind::Arguments(_)),
            ) => true,
            _ => false,
        }
    }
    // https://tc39.github.io/ecma262/#sec-abstract-equality-comparison
    pub fn abstract_equal(self, other: Value) -> Result<bool, RuntimeError> {
        if self.type_equal(&other) {
            return self.strict_equal(other);
        }

        match (&self, &other) {
            (&Value::Null, &Value::Undefined) | (&Value::Undefined, &Value::Null) => Ok(true),
            (&Value::Number(l), &Value::String(_)) => Ok(l == other.to_number()),
            (&Value::String(_), &Value::Number(r)) => Ok(self.to_number() == r),
            (&Value::Bool(_), _) => Ok(Value::Number(self.to_number()).abstract_equal(other)?),
            (_, &Value::Bool(_)) => Ok(Value::Number(other.to_number()).abstract_equal(self)?),
            (&Value::String(_), &Value::Object(_, _))
            | (&Value::Number(_), &Value::Object(_, _)) => {
                Ok(self.abstract_equal(Value::string(other.to_string()))?)
            }
            (&Value::Object(_, _), &Value::String(_))
            | (&Value::Object(_, _), &Value::Number(_)) => {
                Ok(Value::string(self.to_string()).abstract_equal(other)?)
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
    pub fn strict_equal(self, other: Value) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Empty, Value::Empty) => unreachable!(),
            (Value::Null, Value::Null) => Ok(true),
            (Value::Undefined, Value::Undefined) => Ok(true),
            (Value::Bool(l), Value::Bool(r)) => Ok(l == r),
            (Value::Number(l), Value::Number(r)) if l.is_nan() || r.is_nan() => Ok(false),
            (Value::Number(l), Value::Number(r)) => Ok(l == r),
            (Value::String(l), Value::String(r)) => Ok(l == r),
            (Value::Object(l, ObjectKind::Ordinary), Value::Object(r, ObjectKind::Ordinary)) => {
                Ok(l == r)
            }
            (
                Value::Object(l2, ObjectKind::Function(box (FuncInfo { func_id: l1, .. }, _))),
                Value::Object(r2, ObjectKind::Function(box (FuncInfo { func_id: r1, .. }, _))),
            ) => Ok(l1 == r1 && l2 == r2),
            (
                Value::Object(l2, ObjectKind::BuiltinFunction(box (l1, _))),
                Value::Object(r2, ObjectKind::BuiltinFunction(box (r1, _))),
            ) => Ok(l1 == r1 && l2 == r2),
            (Value::Object(_, ObjectKind::Array(l)), Value::Object(_, ObjectKind::Array(r))) => {
                Ok(l == r)
            }
            (
                Value::Object(_, ObjectKind::Arguments(_)),
                Value::Object(_, ObjectKind::Arguments(_)),
            ) => {
                return Err(RuntimeError::Unimplemented);
            }
            _ => Ok(false),
        }
    }
}

impl ArrayValue {
    pub fn new(arr: Vec<Value>) -> ArrayValue {
        let len = arr.len();
        ArrayValue {
            elems: arr.iter().map(|x| x.to_property()).collect(),
            length: len,
        }
    }

    pub fn to_string(&self) -> String {
        self.elems[0..self.length]
            .iter()
            .fold("".to_string(), |acc, prop| {
                acc + prop.val.to_string().as_str() + ","
            })
            .trim_end_matches(",")
            .to_string()
    }

    pub fn push(&mut self, val: Value) {
        self.elems.push(Property::new(val));
        self.length += 1;
    }
}

// Utils

#[inline]
fn is_integer(f: f64) -> bool {
    f - f.floor() == 0.0
}

///
/// get <key> property of <val> object.
/// if the property does not exists, trace the prototype chain.
/// return Value::Undefined for primitives.
/// handle as BuiltinFunction.__proto__ === FUNCTION_PROTOTYPE
///
pub fn obj_find_val(val: Value, key: &str) -> Value {
    let (map, is_builtin_func) = match val {
        Value::Object(map, ObjectKind::BuiltinFunction(_)) => (map, true),
        Value::Object(map, _) => (map, false),
        _ => return Value::Undefined,
    };
    match map.get(key) {
        Some(prop) => prop.val.clone(),
        None if is_builtin_func && key == "__proto__" => {
            return function::FUNCTION_PROTOTYPE.with(|x| x.clone());
        }
        None => match map.get("__proto__") {
            Some(prop) => obj_find_val(prop.val.clone(), key),
            None if is_builtin_func => {
                obj_find_val(function::FUNCTION_PROTOTYPE.with(|x| x.clone()), key)
            }
            _ => return Value::Undefined,
        },
    }
}

///
/// if val is Function or BuiltinFunction, clone val and set this for callobj.this.
/// otherwise, do nothing.
///
pub fn set_this(val: Value, this: &Value) -> Value {
    match val.clone() {
        Value::Object(
            map,
            ObjectKind::Function(box (
                FuncInfo {
                    func_id,
                    iseq,
                    params,
                    ..
                },
                callobj,
            )),
        ) => Value::Object(
            map,
            ObjectKind::Function(Box::new((FuncInfo::new(func_id, iseq, params), {
                let co = CallObject {
                    this: Box::new(this.clone()),
                    ..(*callobj).clone()
                };
                gc::new(co)
            }))),
        ),
        Value::Object(map, ObjectKind::BuiltinFunction(box (id, mut callobj))) => Value::Object(
            map,
            ObjectKind::BuiltinFunction(Box::new((id, {
                *callobj.this = this.clone();
                callobj
            }))),
        ),
        val => val,
    }
}
