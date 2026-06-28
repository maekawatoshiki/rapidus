use super::super::error;
pub use super::array::ArrayObjectInfo;
pub use super::date::DateObjectInfo;
pub use super::error::*;
pub use super::function::*;
pub use super::object::*;
pub use super::prototype::*;
pub use super::symbol::*;
use crate::builtins::BuiltinFuncTy;
use crate::gc;
use crate::vm::exec_context::LexicalEnvironmentRef;
use crate::vm::vm::Factory;
pub use rustc_hash::FxHashMap;

pub const UNINITIALIZED: i32 = 0;
pub const EMPTY: i32 = 1;
pub const NULL: i32 = 2;
pub const UNDEFINED: i32 = 3;
pub const SEPERATOR: i32 = 4;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum PreferredType {
    Default,
    Number,
    String,
}

make_nanbox! {
    #[derive(Clone, PartialEq, Debug, Copy)]
    pub unsafe enum BoxedValue, Value {
        Number(f64),
        Bool(u8), // 0 | 1 = false | true
        String(*mut String),
        Object(*mut Object),
        // Symbol(*mut SymbolInfo),
        Other(i32) // UNINITIALIZED | EMPTY | NULL | UNDEFINED
    }
}

macro_rules! make_property_map_sub {
    ($(
         $property_name:ident,
         $val:expr,
         $writable:ident,
         $enumerable:ident,
         $configurable:ident
    ),*) => { {
        #[allow(unused_mut)]
        let mut record = rustc_hash::FxHashMap::default();
        $( record.insert(
            (stringify!($property_name)).to_string(),
            crate::vm::jsvalue::object::Property::Data(crate::vm::jsvalue::object::DataProperty {
                val: $val,
                writable: $writable,
                enumerable: $enumerable,
                configurable: $configurable
            })
            );
        )*
        record
    } };
}

macro_rules! make_property_order_sub {
    ($(
         $property_name:ident
    ),*) => { {
        let mut order = Vec::new();
        $( order.push((stringify!($property_name)).to_string()); )*
        order
    } };
}

#[macro_export]
macro_rules! make_property_map {
    ($($property_name:ident: $val:expr),*) => { {
        make_property_map_sub!($($property_name, $val, false, false, false),* )
    } };
    ($($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        make_property_map_sub!($($property_name, $val, $x, $y, $z),* )
    } };
}

#[macro_export]
macro_rules! make_property_order {
    ($($property_name:ident: $val:expr),*) => { {
        make_property_order_sub!($($property_name),* )
    } };
    ($($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        make_property_order_sub!($($property_name),* )
    } };
}

#[macro_export]
macro_rules! make_normal_object {
    ($factory:expr) => { {
        Value::Object($factory.alloc(
            crate::vm::jsvalue::object::Object {
                kind: crate::vm::jsvalue::object::ObjectKind::Ordinary,
                prototype: $factory.object_prototypes.object,
                property: rustc_hash::FxHashMap::default(),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: rustc_hash::FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true
            }
        ))
    } };
    ($memory_allocator:expr, $object_prototypes:expr) => { {
        Value::Object($memory_allocator.alloc(
            Object {
                kind: ObjectKind::Ordinary,
                prototype: $object_prototypes.object,
                property: FxHashMap::default(),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true
            }
        ))
    } };
    ($memory_allocator:expr, $object_prototypes:expr, $($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        Value::Object($memory_allocator.alloc(
            Object {
                kind: ObjectKind::Ordinary,
                prototype: $object_prototypes.object,
                property: make_property_map_sub!($($property_name, $val, $x, $y, $z),* ),
                property_order: make_property_order_sub!($($property_name),* ),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true
            }
            ))
    } };
    ($factory:expr, $($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        Value::Object($factory.alloc(
            crate::vm::jsvalue::object::Object {
                kind: crate::vm::jsvalue::object::ObjectKind::Ordinary,
                prototype: $factory.object_prototypes.object,
                property: make_property_map_sub!($($property_name, $val, $x, $y, $z),* ),
                property_order: make_property_order_sub!($($property_name),* ),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: rustc_hash::FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true
            }
            ))
    } };
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Number(number) => write!(f, "{:?}", number),
            Value::Bool(0) => write!(f, "false"),
            Value::Bool(1) => write!(f, "true"),
            Value::Bool(u) => write!(f, "Bool({})", u),
            Value::String(cstr) => write!(f, "{:?}", cstrp_to_str(*cstr)),
            Value::Other(UNINITIALIZED) => write!(f, "UNINITIALIZED"),
            Value::Other(EMPTY) => write!(f, "UNINITIALIZED"),
            Value::Other(NULL) => write!(f, "NULL"),
            Value::Other(UNDEFINED) => write!(f, "UNDEFINED"),
            Value::Other(SEPERATOR) => write!(f, "SEPERATER"),
            Value::Other(i) => write!(f, "Other({})", i),
            Value::Object(ref info) => {
                let info = ObjectRef(*info);
                match info.kind {
                    ObjectKind::Ordinary => write!(f, "Object"),
                    ObjectKind::Arguments(_) => write!(f, "Arguments"),
                    ObjectKind::Function(_) => write!(f, "Function"),
                    ObjectKind::Array(_) => write!(f, "Array"),
                    ObjectKind::Date(_) => write!(f, "Date"),
                    ObjectKind::RegExp(_) => write!(f, "RegExp"),
                    ObjectKind::Map(_) => write!(f, "Map"),
                    ObjectKind::Set(_) => write!(f, "Set"),
                    ObjectKind::WeakMap(_) => write!(f, "WeakMap"),
                    ObjectKind::WeakSet(_) => write!(f, "WeakSet"),
                    ObjectKind::WeakRef(_) => write!(f, "WeakRef"),
                    ObjectKind::FinalizationRegistry(_) => write!(f, "FinalizationRegistry"),
                    ObjectKind::ShadowRealm(_) => write!(f, "ShadowRealm"),
                    ObjectKind::MapIterator(_) => write!(f, "Map Iterator"),
                    ObjectKind::SetIterator(_) => write!(f, "Set Iterator"),
                    ObjectKind::Generator(_) => write!(f, "Generator"),
                    ObjectKind::ArrayBuffer(ref info) if info.shared => {
                        write!(f, "SharedArrayBuffer")
                    }
                    ObjectKind::ArrayBuffer(_) => write!(f, "ArrayBuffer"),
                    ObjectKind::DataView(_) => write!(f, "DataView"),
                    ObjectKind::TypedArray(ref info) => write!(f, "{}", info.name),
                    ObjectKind::Symbol(_) => write!(f, "Symbol"),
                    ObjectKind::BigInt(ref info) => write!(f, "{}n", info.decimal),
                    ObjectKind::Error(_) => write!(f, "Error"),
                    ObjectKind::Proxy(_) => write!(f, "Proxy"),
                    ObjectKind::Temporal(_) => write!(f, "Temporal"),
                }
            }
        }
    }
}

impl Value {
    #[inline]
    pub const fn null() -> Self {
        Value::Other(NULL)
    }

    #[inline]
    pub const fn undefined() -> Self {
        Value::Other(UNDEFINED)
    }

    #[inline]
    pub const fn uninitialized() -> Self {
        Value::Other(UNINITIALIZED)
    }

    #[inline]
    pub const fn empty() -> Self {
        Value::Other(EMPTY)
    }

    #[inline]
    pub const fn seperator() -> Self {
        Value::Other(SEPERATOR)
    }

    #[inline]
    pub fn bool(x: bool) -> Self {
        Value::Bool(if x { 1 } else { 0 })
    }

    fn string(memory_allocator: &mut gc::MemoryAllocator, body: String) -> Self {
        Value::String(memory_allocator.alloc(body))
    }

    pub fn builtin_function_with_proto(
        memory_allocator: &mut gc::MemoryAllocator,
        proto: Value,
        name: impl Into<String>,
        func: BuiltinFuncTy,
    ) -> Self {
        let name: String = name.into();
        let name_prop = Value::string(memory_allocator, name.clone());
        Value::Object(memory_allocator.alloc(Object {
            kind: ObjectKind::Function(FunctionObjectInfo {
                name: Some(name),
                super_constructor: None,
                kind: FunctionObjectKind::Builtin(func),
            }),
            prototype: proto,
            property: make_property_map!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            property_order: make_property_order!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }
}

impl Value {
    pub fn is_undefined(&self) -> bool {
        match self {
            Value::Other(UNDEFINED) => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Value::Other(NULL) => true,
            _ => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Value::Other(EMPTY) => true,
            _ => false,
        }
    }

    pub fn is_seperator(&self) -> bool {
        match self {
            Value::Other(SEPERATOR) => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Value::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_function_object(&self) -> bool {
        match self {
            Value::Object(info) => match ObjectRef(*info).kind {
                ObjectKind::Function(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_array_object(&self) -> bool {
        match self {
            Value::Object(info) => match ObjectRef(*info).kind {
                ObjectKind::Array(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_date_object(&self) -> bool {
        match self {
            Value::Object(info) => match ObjectRef(*info).kind {
                ObjectKind::Date(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    /// Returns true if the value is an Error object.
    pub fn is_error_object(&self) -> bool {
        match self {
            Value::Object(info) => match ObjectRef(*info).kind {
                ObjectKind::Error(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Value::Object(info) => match ObjectRef(*info).kind {
                ObjectKind::Symbol(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_bigint(&self) -> bool {
        match self {
            Value::Object(info) => matches!(ObjectRef(*info).kind, ObjectKind::BigInt(_)),
            _ => false,
        }
    }

    pub fn bigint_decimal(&self) -> Option<String> {
        match self {
            Value::Object(info) => match ObjectRef(*info).kind {
                ObjectKind::BigInt(ref info) => Some(info.decimal.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-canonicalnumericindexstring
    pub fn is_canonical_numeric_index_string(
        &self,
        allocator: &mut gc::MemoryAllocator,
    ) -> Option<usize> {
        if !self.is_string() {
            return None;
        }
        let s = self.into_str();
        let num = self.to_number(allocator);
        if s == Value::Number(num).to_string() && is_integer(num) && num >= 0.0 {
            Some(num as usize)
        } else {
            None
        }
    }

    pub fn is_array_index(&self) -> Option<usize> {
        if !self.is_number() {
            return None;
        }
        let num = self.into_number();
        if is_integer(num) && 0.0 <= num && num < 4294967295.0 {
            Some(num as usize)
        } else {
            None
        }
    }
}

impl Value {
    pub fn has_own_property(&self, key: &str) -> bool {
        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).has_own_property(key),
            _ => false,
        }
    }

    pub fn get_prototype(&self) -> Value {
        match self {
            Value::Object(info) => ObjectRef(*info).get_prototype(),
            _ => Value::undefined(),
        }
    }

    pub fn get_object_properties(&self) -> Option<&FxHashMap<String, Property>> {
        match self {
            Value::Object(obj_info) => Some(&unsafe { &**obj_info }.property),
            _ => None,
        }
    }

    pub fn get_property(&self, key: &str) -> Value {
        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).get_property(key),
            _ => Value::undefined(),
        }
    }

    pub fn get_property_by_value(
        &self,
        factory: &mut Factory,
        key: Value,
    ) -> Result<Property, error::RuntimeError> {
        fn string_get_property(
            factory: &mut Factory,
            s: &str,
            key: Value,
        ) -> Result<Property, error::RuntimeError> {
            match key {
                Value::Number(idx) if is_integer(idx) => Ok(Property::new_data_simple(
                    factory.string(s.chars().nth(idx as usize).unwrap().to_string()),
                )),
                Value::String(x) if cstrp_to_str(x) == "length" => Ok(Property::new_data_simple(
                    Value::Number(s.chars().fold(0, |x, c| x + c.len_utf16()) as f64),
                )),
                key => factory
                    .object_prototypes
                    .string
                    .get_object_info()
                    .get_property_by_value(factory, key),
            }
        }

        match self {
            Value::String(s) => {
                return string_get_property(factory, cstrp_to_str(*s), key);
            }
            Value::Number(_) => {
                return factory
                    .object_prototypes
                    .number
                    .get_object_info()
                    .get_property_by_value(factory, key);
            }
            Value::Object(info) if matches!(ObjectRef(*info).kind, ObjectKind::BigInt(_)) => {
                return factory
                    .object_prototypes
                    .bigint
                    .get_object_info()
                    .get_property_by_value(factory, key);
            }
            Value::Bool(_) => {
                return factory
                    .object_prototypes
                    .boolean
                    .get_object_info()
                    .get_property_by_value(factory, key);
            }
            Value::Other(_) => {
                return Err(error::RuntimeError::typeerr(format!(
                    "TypeError: Cannot read property '{}' of {}",
                    key.to_string(),
                    self.to_string()
                )));
            }
            _ => {}
        }

        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).get_property_by_value(factory, key),
            _ => Ok(Property::new_data_simple(Value::undefined())),
        }
    }

    pub fn set_property(&self, key: impl Into<String>, val: Value) {
        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).set_property(key.into(), val),
            _ => {}
        }
    }

    pub fn set_property_by_value(
        &self,
        allocator: &mut gc::MemoryAllocator,
        key: Value,
        val: Value,
    ) -> Result<(Option<Value>, bool), error::RuntimeError> {
        match self {
            Value::Object(obj_info) => {
                ObjectRef(*obj_info).set_property_by_value(allocator, key, val)
            }
            Value::Other(_) => Err(error::RuntimeError::typeerr(format!(
                "TypeError: Cannot set property '{}' of {}",
                key.to_string(),
                self.to_string()
            ))),
            _ => Ok((None, false)),
        }
    }

    pub fn delete_property_by_value(
        &self,
        allocator: &mut gc::MemoryAllocator,
        key: Value,
    ) -> Result<bool, error::RuntimeError> {
        match self {
            Value::Object(obj_info) => {
                ObjectRef(*obj_info).delete_property_by_value(allocator, key)
            }
            Value::Other(_) => Err(error::RuntimeError::typeerr(format!(
                "TypeError: Cannot delete property '{}' of {}",
                key.to_string(),
                self.to_string()
            ))),
            _ => Ok(true),
        }
    }

    pub fn set_constructor(&self, val: Value) {
        self.get_object_info().property.insert(
            "constructor".to_string(),
            Property::Data(DataProperty {
                val,
                writable: true,
                enumerable: false,
                configurable: true,
            }),
        );
    }

    pub fn set_function_outer_environment(&mut self, env: LexicalEnvironmentRef) {
        match self {
            Value::Object(obj) => match ObjectRef(*obj).kind {
                ObjectKind::Function(ref mut info) => info.set_outer_environment(env),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn copy_object(&self, memory_allocator: &mut gc::MemoryAllocator) -> Value {
        match self {
            Value::Object(obj) => {
                let obj = (*ObjectRef(*obj)).clone();
                Value::Object(memory_allocator.alloc(obj))
            }
            e => *e,
        }
    }

    pub fn as_function(&self) -> &FunctionObjectInfo {
        match self {
            Value::Object(obj) => {
                let obj = unsafe { &**obj };
                match obj.kind {
                    ObjectKind::Function(ref info) => &info,
                    _ => panic!(),
                }
            }
            e => panic!("{:?}", e),
        }
    }

    pub fn as_date(&self) -> Option<&DateObjectInfo> {
        match self {
            Value::Object(obj) => {
                let obj = unsafe { &**obj };
                match obj.kind {
                    ObjectKind::Date(ref info) => Some(&info),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn as_date_mut(&self) -> Option<&mut DateObjectInfo> {
        match self {
            Value::Object(obj) => {
                let obj = unsafe { &mut **obj };
                match obj.kind {
                    ObjectKind::Date(ref mut info) => Some(info),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn as_array_mut(&self) -> &mut ArrayObjectInfo {
        match self {
            Value::Object(obj) => {
                let obj = unsafe { &mut **obj };
                match obj.kind {
                    ObjectKind::Array(ref mut info) => return info,
                    _ => panic!(),
                }
            }
            e => panic!("{:?}", e),
        }
    }

    pub fn get_object_info(&self) -> ObjectRef {
        match self {
            Value::Object(obj) => ObjectRef(*obj),
            _ => panic!(),
        }
    }

    pub fn get_symbol_info(&self) -> &mut SymbolInfo {
        match self {
            Value::Object(info) => match unsafe { &mut **info }.kind {
                ObjectKind::Symbol(ref mut info) => info,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn into_number(self) -> f64 {
        match self {
            Value::Number(x) => x,
            _ => panic!(),
        }
    }

    pub fn into_str(self) -> &'static str {
        match self {
            Value::String(s) => cstrp_to_str(s),
            _ => panic!(),
        }
    }

    pub fn into_bool(self) -> bool {
        match self {
            Value::Bool(b) => {
                if b == 1 {
                    true
                } else {
                    false
                }
            }
            _ => panic!(),
        }
    }

    pub fn to_undefined_if_empty(self) -> Value {
        if self == Value::empty() {
            return Value::undefined();
        }
        self
    }
}

impl Value {
    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-tonumber
    pub fn to_number(&self, allocator: &mut gc::MemoryAllocator) -> f64 {
        match self {
            Value::Other(UNDEFINED) => ::std::f64::NAN,
            Value::Other(NULL) => 0.0,
            Value::Bool(0) => 0.0,
            Value::Bool(1) => 1.0,
            Value::Number(n) => *n,
            Value::Object(info) if matches!(ObjectRef(*info).kind, ObjectKind::BigInt(_)) => {
                match ObjectRef(*info).kind {
                    ObjectKind::BigInt(ref info) => info.decimal.parse::<f64>().unwrap_or(f64::NAN),
                    _ => unreachable!(),
                }
            }
            Value::String(s) => {
                let s = cstrp_to_str(*s).trim();
                if s == "Infinity" || s == "+Infinity" {
                    ::std::f64::INFINITY
                } else if s == "-Infinity" {
                    ::std::f64::NEG_INFINITY
                } else if s.len() == 0 {
                    0.0
                } else if s.chars().all(|c| c.is_whitespace()) {
                    0.0
                } else if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
                    u64::from_str_radix(hex, 16)
                        .map(|num| num as f64)
                        .unwrap_or(::std::f64::NAN)
                } else if let Some(bin) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
                    u64::from_str_radix(bin, 2)
                        .map(|num| num as f64)
                        .unwrap_or(::std::f64::NAN)
                } else if let Some(oct) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
                    u64::from_str_radix(oct, 8)
                        .map(|num| num as f64)
                        .unwrap_or(::std::f64::NAN)
                } else {
                    if s.chars()
                        .any(|ch| ch.is_ascii_alphabetic() && ch != 'e' && ch != 'E')
                    {
                        ::std::f64::NAN
                    } else {
                        s.parse::<f64>().unwrap_or(::std::f64::NAN)
                    }
                }
            }
            Value::Object(_) => self
                .to_primitive(allocator, Some(PreferredType::Number))
                .to_number(allocator),
            // TODO
            _ => 0.0,
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-tostring
    pub fn to_string(&self) -> String {
        match self {
            Value::Bool(0) => "false".to_string(),
            Value::Bool(1) => "true".to_string(),
            Value::String(s) => cstrp_to_str(*s).to_string(),
            Value::Other(UNDEFINED) => "undefined".to_string(),
            Value::Other(NULL) => "null".to_string(),
            Value::Number(n) => {
                if n.is_nan() {
                    "NaN".to_string()
                } else if n.is_infinite() {
                    if n.is_sign_negative() {
                        "-Infinity".to_string()
                    } else {
                        "Infinity".to_string()
                    }
                } else if *n == 0.0 {
                    "0".to_string()
                } else if n.abs() >= 1e21 || n.abs() < 1e-6 {
                    number_to_exponential_string(*n)
                } else {
                    format!("{}", n)
                }
            }
            Value::Object(info) if matches!(ObjectRef(*info).kind, ObjectKind::BigInt(_)) => {
                match ObjectRef(*info).kind {
                    ObjectKind::BigInt(ref info) => info.decimal.clone(),
                    _ => unreachable!(),
                }
            }
            Value::Object(info) => {
                let info = ObjectRef(*info);
                match info.kind {
                    ObjectKind::Ordinary => "[object Object]".to_string(),
                    ObjectKind::Array(ref info) => info.join(None),
                    ObjectKind::WeakRef(_) => "[object WeakRef]".to_string(),
                    ObjectKind::FinalizationRegistry(_) => {
                        "[object FinalizationRegistry]".to_string()
                    }
                    ObjectKind::ShadowRealm(_) => "[object ShadowRealm]".to_string(),
                    _ => "[unimplemented]".to_string(), // TODO
                }
            }
            _ => "[unimplemented]".to_string(),
        }
    }

    pub fn to_boolean(&self) -> bool {
        match self {
            Value::Bool(0) => false,
            Value::Bool(1) => true,
            Value::Other(NULL) => false,
            Value::Other(UNDEFINED) => false,
            Value::Number(num) => {
                if *num == 0f64 || num.is_nan() {
                    false
                } else {
                    true
                }
            }
            Value::Object(info) if matches!(ObjectRef(*info).kind, ObjectKind::BigInt(_)) => {
                match ObjectRef(*info).kind {
                    ObjectKind::BigInt(ref info) => info.decimal != "0",
                    _ => unreachable!(),
                }
            }
            Value::String(s) => cstrp_to_str(*s).len() != 0,
            _ => true,
        }
    }

    /// https://tc39.github.io/ecma262/#sec-toint32
    pub fn to_int32(&self, allocator: &mut gc::MemoryAllocator) -> i32 {
        let number = self.to_number(allocator);
        match number {
            number if number.is_nan() || number == 0.0 || number.is_infinite() => 0,
            number => (number.trunc()) as i32,
        }
    }

    /// https://tc39.github.io/ecma262/#sec-touint32
    pub fn to_uint32(&self, allocator: &mut gc::MemoryAllocator) -> u32 {
        let number = self.to_number(allocator);
        match number {
            number if number.is_nan() || number == 0.0 || number.is_infinite() => 0,
            number => (number.trunc()) as u32,
        }
    }

    /// https://tc39.github.io/ecma262/#sec-toprimitive
    pub fn to_primitive(
        &self,
        allocator: &mut gc::MemoryAllocator,
        preferred_type: Option<PreferredType>,
    ) -> Value {
        if self.is_symbol() || self.is_bigint() {
            return *self;
        }
        if !self.is_object() {
            return *self;
        }

        let mut hint = preferred_type.unwrap_or(PreferredType::Default);

        // TODO: Call @@toPrimitive if present

        if hint == PreferredType::Default {
            hint = PreferredType::Number
        }

        self.ordinary_to_primitive(allocator, hint)
    }

    /// https://tc39.github.io/ecma262/#sec-ordinarytoprimitive
    pub fn ordinary_to_primitive(
        &self,
        allocator: &mut gc::MemoryAllocator,
        hint: PreferredType,
    ) -> Value {
        match hint {
            PreferredType::Number => {
                if let Some(val) = self.value_of() {
                    if !val.is_object() || val.is_symbol() || val.is_bigint() {
                        return val;
                    }
                }

                Value::string(allocator, self.to_string())
            }
            PreferredType::String => Value::string(allocator, self.to_string()),
            PreferredType::Default => unreachable!(),
        }
    }

    pub fn value_of(self) -> Option<Value> {
        match self {
            Value::Object(info) => {
                let info = unsafe { &*info };
                match info.kind {
                    ObjectKind::Ordinary => {
                        for internal in [
                            "__string_data",
                            "__number_data",
                            "__boolean_data",
                            "__symbol_data",
                            "__bigint_data",
                        ] {
                            if let Some(prop) = info.property.get(internal) {
                                if let Some(data) = prop.get_data() {
                                    return Some(data.val);
                                }
                            }
                        }
                        Some(self)
                    }
                    ObjectKind::Arguments(_) => Some(self),
                    ObjectKind::Function(_) => None,
                    ObjectKind::Array(_) => None,
                    ObjectKind::Date(_) => None,
                    ObjectKind::RegExp(_)
                    | ObjectKind::Map(_)
                    | ObjectKind::Set(_)
                    | ObjectKind::WeakMap(_)
                    | ObjectKind::WeakSet(_)
                    | ObjectKind::WeakRef(_)
                    | ObjectKind::FinalizationRegistry(_)
                    | ObjectKind::ShadowRealm(_)
                    | ObjectKind::MapIterator(_)
                    | ObjectKind::SetIterator(_)
                    | ObjectKind::Generator(_)
                    | ObjectKind::ArrayBuffer(_)
                    | ObjectKind::DataView(_)
                    | ObjectKind::TypedArray(_) => Some(self),
                    ObjectKind::Error(_) => None,
                    ObjectKind::Symbol(_) => Some(self), // TODO
                    ObjectKind::BigInt(_) => Some(self),
                    ObjectKind::Proxy(_) => Some(self),
                    ObjectKind::Temporal(_) => Some(self),
                }
            }
            Value::String(_) => Some(self), // TODO
            _ => None,
        }
    }
}

impl Value {
    fn bigint_from_i128(allocator: &mut gc::MemoryAllocator, value: i128) -> Value {
        Value::Object(allocator.alloc(Object {
            kind: ObjectKind::BigInt(BigIntInfo {
                decimal: value.to_string(),
            }),
            prototype: Value::undefined(),
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    fn bigint_i128(&self) -> Option<i128> {
        self.bigint_decimal()?.parse::<i128>().ok()
    }

    fn bigint_binary_i128(
        allocator: &mut gc::MemoryAllocator,
        lhs: Value,
        rhs: Value,
        op: impl FnOnce(i128, i128) -> Option<i128>,
    ) -> Value {
        match (lhs.bigint_i128(), rhs.bigint_i128()) {
            (Some(l), Some(r)) => op(l, r)
                .map(|value| Value::bigint_from_i128(allocator, value))
                .unwrap_or_else(Value::undefined),
            _ => Value::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-addition-operator-plus-runtime-semantics-evaluation
    pub fn add(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        let lprim = self.to_primitive(allocator, None);
        let rprim = val.to_primitive(allocator, None);
        match (lprim, rprim) {
            (x, y) if x.is_bigint() && y.is_bigint() => {
                Value::bigint_binary_i128(allocator, x, y, |l, r| l.checked_add(r))
            }
            (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
            (Value::String(x), Value::String(y)) => {
                let x = cstrp_to_str(x);
                let y = cstrp_to_str(y);
                let cat = format!("{}{}", x, y);
                Value::string(allocator, cat)
            }
            (Value::String(x), _) => {
                let x = cstrp_to_str(x);
                Value::string(allocator, format!("{}{}", x, rprim.to_string()))
            }
            (_, Value::String(y)) => {
                let y = cstrp_to_str(y);
                Value::string(allocator, format!("{}{}", lprim.to_string(), y))
            }
            (x, y) if x.is_bigint() || y.is_bigint() => Value::undefined(),
            (x, y) => Value::Number(x.to_number(allocator) + y.to_number(allocator)),
        }
    }

    // https://www.ecma-international.org/ecma-262/6.0/#sec-subtraction-operator-minus-runtime-semantics-evaluation
    pub fn sub(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        match (self, val) {
            (x, y) if x.is_bigint() && y.is_bigint() => {
                Value::bigint_binary_i128(allocator, x, y, |l, r| l.checked_sub(r))
            }
            (x, y) if x.is_bigint() || y.is_bigint() => Value::undefined(),
            (Value::Number(x), Value::Number(y)) => Value::Number(x - y),
            (x, y) => Value::Number(x.to_number(allocator) - y.to_number(allocator)),
        }
    }

    pub fn mul(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        match (self, val) {
            (x, y) if x.is_bigint() && y.is_bigint() => {
                Value::bigint_binary_i128(allocator, x, y, |l, r| l.checked_mul(r))
            }
            (x, y) if x.is_bigint() || y.is_bigint() => Value::undefined(),
            (Value::Number(x), Value::Number(y)) => Value::Number(x * y),
            _ => Value::undefined(),
        }
    }

    pub fn div(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        match (self, val) {
            (x, y) if x.is_bigint() && y.is_bigint() => {
                Value::bigint_binary_i128(allocator, x, y, |l, r| {
                    if r == 0 {
                        None
                    } else {
                        l.checked_div(r)
                    }
                })
            }
            (x, y) if x.is_bigint() || y.is_bigint() => Value::undefined(),
            (Value::Number(x), Value::Number(y)) => Value::Number(x / y),
            _ => Value::undefined(),
        }
    }

    pub fn rem(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        match (self, val) {
            (x, y) if x.is_bigint() && y.is_bigint() => {
                Value::bigint_binary_i128(allocator, x, y, |l, r| {
                    if r == 0 {
                        None
                    } else {
                        l.checked_rem(r)
                    }
                })
            }
            (x, y) if x.is_bigint() || y.is_bigint() => Value::undefined(),
            (Value::Number(x), Value::Number(y)) => {
                let result = x % y;
                Value::Number(if result.is_nan() { f64::NAN } else { result })
            }
            _ => Value::undefined(),
        }
    }

    pub fn exp(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() && val.is_bigint() {
            let Some(base) = self.bigint_i128() else {
                return Value::undefined();
            };
            let Some(exp) = val.bigint_i128() else {
                return Value::undefined();
            };
            if exp < 0 || exp > u32::MAX as i128 {
                return Value::undefined();
            }
            return base
                .checked_pow(exp as u32)
                .map(|value| Value::bigint_from_i128(allocator, value))
                .unwrap_or_else(Value::undefined);
        }
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number(self.to_number(allocator).powf(val.to_number(allocator)))
    }

    pub fn and(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() && val.is_bigint() {
            return Value::bigint_binary_i128(allocator, self, val, |l, r| Some(l & r));
        }
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number((self.to_int32(allocator) & val.to_int32(allocator)) as f64)
    }

    pub fn or(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() && val.is_bigint() {
            return Value::bigint_binary_i128(allocator, self, val, |l, r| Some(l | r));
        }
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number((self.to_int32(allocator) | val.to_int32(allocator)) as f64)
    }

    pub fn xor(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() && val.is_bigint() {
            return Value::bigint_binary_i128(allocator, self, val, |l, r| Some(l ^ r));
        }
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number((self.to_int32(allocator) ^ val.to_int32(allocator)) as f64)
    }

    pub fn not(self, allocator: &mut gc::MemoryAllocator) -> Self {
        if self.is_bigint() {
            return self
                .bigint_i128()
                .map(|value| Value::bigint_from_i128(allocator, !value))
                .unwrap_or_else(Value::undefined);
        }
        Value::Number((!self.to_int32(allocator)) as f64)
    }

    /// https://tc39.github.io/ecma262/#sec-left-shift-operator
    pub fn shift_l(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() && val.is_bigint() {
            return Value::bigint_binary_i128(allocator, self, val, |l, r| {
                if r < 0 || r > u32::MAX as i128 {
                    None
                } else {
                    l.checked_shl(r as u32)
                }
            });
        }
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number((self.to_int32(allocator) << (val.to_uint32(allocator) & 0x1f)) as f64)
    }

    /// https://tc39.github.io/ecma262/#sec-signed-right-shift-operator
    pub fn shift_r(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() && val.is_bigint() {
            return Value::bigint_binary_i128(allocator, self, val, |l, r| {
                if r < 0 || r > u32::MAX as i128 {
                    None
                } else {
                    l.checked_shr(r as u32)
                }
            });
        }
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number((self.to_int32(allocator) >> (val.to_uint32(allocator) & 0x1f)) as f64)
    }

    /// https://tc39.github.io/ecma262/#sec-unsigned-right-shift-operator
    pub fn z_shift_r(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_bigint() || val.is_bigint() {
            return Value::undefined();
        }
        Value::Number((self.to_uint32(allocator) >> (val.to_uint32(allocator) & 0x1f)) as f64)
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-abstract-equality-comparison
    pub fn eq(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        if self.is_same_type_as(&val) {
            return self.strict_eq(val);
        }

        match (self, val) {
            (Value::Other(NULL), Value::Other(UNDEFINED)) => return Value::bool(true),
            (Value::Other(UNDEFINED), Value::Other(NULL)) => return Value::bool(true),
            _ => {}
        }

        match (self, val) {
            (x, y) if x.is_bigint() && y.is_bigint() => {
                Value::bool(x.bigint_decimal() == y.bigint_decimal())
            }
            (x, y) if x.is_symbol() || y.is_symbol() => Value::bool(false),
            (Value::Number(x), Value::String(_)) => Value::bool(x == val.to_number(allocator)),
            (Value::String(_), Value::Number(y)) => Value::bool(self.to_number(allocator) == y),
            (Value::Bool(_), Value::Number(y)) => Value::bool(self.to_number(allocator) == y),
            (Value::Number(x), Value::Bool(_)) => Value::bool(x == val.to_number(allocator)),
            // (Value::Number(x), Value::Number(y)) => Value::Bool(if x == y { 1 } else { 0 }),
            // (Value::Number(_), obj) | (Value::String(_), obj) => self.eq(val),
            (Value::Object(_), _) => self.to_primitive(allocator, None).eq(allocator, val),
            (_, Value::Object(_)) => val.to_primitive(allocator, None).eq(allocator, self),
            _ => Value::bool(false),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-strict-equality-comparison
    pub fn strict_eq(self, val: Value) -> Value {
        Value::bool(self.strict_eq_bool(val))
    }

    pub fn strict_eq_bool(self, val: Value) -> bool {
        fn get_obj_ptr(val: Value) -> u64 {
            match val {
                Value::Object(obj) => obj as u64,
                _ => panic!(),
            }
        }

        if !self.is_same_type_as(&val) {
            return false;
        }

        if self == Value::undefined() || self == Value::null() {
            return true;
        }

        match self {
            Value::Number(_) => self.into_number() == val.into_number(),
            Value::String(_) => self.into_str() == val.into_str(),
            Value::Bool(_) => self.into_bool() == val.into_bool(),
            Value::Object(info) if matches!(ObjectRef(info).kind, ObjectKind::BigInt(_)) => {
                self.bigint_decimal() == val.bigint_decimal()
            }
            Value::Object(_) => get_obj_ptr(self) == get_obj_ptr(val),
            _ => false,
        }
    }

    pub fn ne(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::bool(!self.eq(allocator, val).into_bool())
    }

    pub fn strict_ne(self, val: Value) -> Self {
        Value::bool(!self.strict_eq(val).into_bool())
    }

    /// https://tc39.github.io/ecma262/#sec-abstract-relational-comparison
    pub fn cmp(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        let px = self.to_primitive(allocator, None);
        let py = val.to_primitive(allocator, None);

        if let (Value::String(x), Value::String(y)) = (px, py) {
            return Value::bool(cstrp_to_str(x) < cstrp_to_str(y));
        }

        if px.is_bigint() && py.is_bigint() {
            return match (px.bigint_i128(), py.bigint_i128()) {
                (Some(x), Some(y)) => Value::bool(x < y),
                _ => Value::undefined(),
            };
        }
        if px.is_bigint() || py.is_bigint() {
            return Value::undefined();
        }

        let nx = px.to_number(allocator);
        let ny = py.to_number(allocator);

        if nx.is_nan() || ny.is_nan() {
            return Value::undefined();
        }

        if nx == ny {
            return Value::Bool(0);
        }

        Value::bool(nx < ny)
    }

    pub fn lt(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        match self.cmp(allocator, val) {
            Value::Other(UNDEFINED) => Value::Bool(0),
            otherwise => otherwise,
        }
    }

    pub fn le(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        match val.cmp(allocator, self) {
            Value::Other(UNDEFINED) | Value::Bool(1) => Value::Bool(0),
            _ => Value::Bool(1),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-unary-minus-operator-runtime-semantics-evaluation
    pub fn minus(self, allocator: &mut gc::MemoryAllocator) -> Self {
        match self {
            Value::Number(n) => Value::Number(-n),
            value if value.is_bigint() => {
                let Some(decimal) = value.bigint_decimal() else {
                    return Value::undefined();
                };
                let decimal = if decimal == "0" {
                    decimal
                } else if let Some(positive) = decimal.strip_prefix('-') {
                    positive.to_string()
                } else {
                    format!("-{}", decimal)
                };
                Value::Object(allocator.alloc(Object {
                    kind: ObjectKind::BigInt(BigIntInfo { decimal }),
                    prototype: Value::undefined(),
                    property: FxHashMap::default(),
                    property_order: Vec::new(),
                    private_elements: FxHashMap::default(),
                    sym_property: FxHashMap::default(),
                    sym_property_order: Vec::new(),
                    extensible: true,
                }))
            }
            _ => Value::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-unary-plus-operator-runtime-semantics-evaluation
    pub fn positive(self, allocator: &mut gc::MemoryAllocator) -> Self {
        if self.is_bigint() {
            return Value::undefined();
        }
        Value::Number(self.to_number(allocator))
    }

    pub fn is_same_type_as(&self, val: &Value) -> bool {
        match (self, val) {
            (Value::Other(UNINITIALIZED), Value::Other(UNINITIALIZED))
            | (Value::Other(EMPTY), Value::Other(EMPTY))
            | (Value::Other(NULL), Value::Other(NULL))
            | (Value::Other(UNDEFINED), Value::Other(UNDEFINED))
            | (Value::Number(_), Value::Number(_))
            | (Value::String(_), Value::String(_))
            | (Value::Bool(_), Value::Bool(_)) => true,
            (Value::Object(_), Value::Object(_)) => {
                self.is_bigint() == val.is_bigint() && self.is_symbol() == val.is_symbol()
            }
            _ => false,
        }
    }

    // TODO: Correct implementation: https://www.ecma-international.org/ecma-262/6.0/#sec-typeof-operator-runtime-semantics-evaluation
    pub fn type_of(&self) -> &str {
        match self {
            Value::Other(UNDEFINED) => "undefined",
            Value::Other(NULL) => "object",
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            value if value.is_bigint() => "bigint",
            Value::String(_) => "string",
            Value::Object(info) => {
                let info = ObjectRef(*info);
                match info.kind {
                    ObjectKind::Function(_) => "function",
                    ObjectKind::Array(_) => "object",
                    ObjectKind::Date(_) => "object",
                    ObjectKind::RegExp(_)
                    | ObjectKind::Map(_)
                    | ObjectKind::Set(_)
                    | ObjectKind::WeakMap(_)
                    | ObjectKind::WeakSet(_)
                    | ObjectKind::WeakRef(_)
                    | ObjectKind::FinalizationRegistry(_)
                    | ObjectKind::ShadowRealm(_)
                    | ObjectKind::MapIterator(_)
                    | ObjectKind::SetIterator(_)
                    | ObjectKind::Generator(_)
                    | ObjectKind::ArrayBuffer(_)
                    | ObjectKind::DataView(_)
                    | ObjectKind::TypedArray(_) => "object",
                    ObjectKind::Symbol(_) => "symbol",
                    ObjectKind::BigInt(_) => "bigint",
                    ObjectKind::Error(_) => "object",
                    ObjectKind::Arguments(_) => "object",
                    ObjectKind::Proxy(ref proxy) if proxy.target.type_of() == "function" => {
                        "function"
                    }
                    ObjectKind::Proxy(_) => "object",
                    ObjectKind::Temporal(_) => "object",
                    ObjectKind::Ordinary => "object",
                }
            }
            _ => panic!(),
        }
    }
}

impl Value {
    pub fn debug_string(&self, nest: bool) -> String {
        fn property_string(sorted_key_val: Vec<(&String, &Property)>) -> String {
            sorted_key_val
                .iter()
                .enumerate()
                .fold("".to_string(), |acc, (i, tupple)| {
                    format!(
                        "{}'{}': {}{}",
                        acc,
                        tupple.0,
                        match tupple.1 {
                            Property::Data(DataProperty { val, .. }) => val.debug_string(true),
                            Property::Accessor(AccessorProperty { get, set, .. }) => {
                                let s_get = if get.is_undefined() { "" } else { "Getter" };
                                let s_set = if set.is_undefined() { "" } else { "Setter" };
                                format!(
                                    "[{}{}{}]",
                                    s_get,
                                    if !get.is_undefined() && !set.is_undefined() {
                                        "/"
                                    } else {
                                        ""
                                    },
                                    s_set
                                )
                            }
                        },
                        if i != sorted_key_val.len() - 1 {
                            ", "
                        } else {
                            " "
                        }
                    )
                })
        }

        match self {
            Value::Other(UNINITIALIZED) => "uninitialized".to_string(),
            Value::Other(EMPTY) => "empty".to_string(),
            Value::Other(NULL) => "null".to_string(),
            Value::Other(UNDEFINED) => "undefined".to_string(),
            Value::Other(_) => unreachable!(),
            Value::Bool(1) => "true".to_string(),
            Value::Bool(0) => "false".to_string(),
            Value::Bool(_) => unreachable!(),
            Value::Number(n) => {
                if n.is_nan() {
                    "NaN".to_string()
                } else if n.is_infinite() {
                    "Infinity".to_string()
                } else {
                    format!("{}", n)
                }
            }
            Value::String(s) => {
                let s = cstrp_to_str(*s);
                if nest {
                    format!("'{}'", s)
                } else {
                    s.to_string()
                }
            }
            Value::Object(obj_info) => {
                let obj_info = ObjectRef(*obj_info);
                match obj_info.kind {
                    ObjectKind::Ordinary => {
                        let mut sorted_key_val =
                            (&obj_info.property)
                                .iter()
                                .collect::<Vec<(&String, &Property)>>();
                        sorted_key_val
                            .sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                        format!("{{ {} }}", property_string(sorted_key_val))
                    }
                    ObjectKind::Arguments(_) => {
                        let mut sorted_key_val =
                            (&obj_info.property)
                                .iter()
                                .collect::<Vec<(&String, &Property)>>();
                        sorted_key_val
                            .sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                        format!("[Arguments] {{ {} }}", property_string(sorted_key_val))
                    }
                    ObjectKind::Symbol(ref info) => format!(
                        "Symbol({})",
                        info.description.as_ref().unwrap_or(&"".to_string())
                    ),
                    ObjectKind::BigInt(ref info) => format!("{}n", info.decimal),
                    ObjectKind::Error(ref _info) => {
                        format!("Error({})", obj_info.get_property("message").to_string())
                    }
                    ObjectKind::Function(ref func_info) => {
                        if let Some(ref name) = func_info.name {
                            format!("[Function: {}]", name)
                        } else {
                            "[Function]".to_string()
                        }
                    }
                    ObjectKind::Date(ref date) => date.to_string(),
                    ObjectKind::RegExp(ref info) => {
                        format!("/{}/{}", info.original_source, info.original_flags)
                    }
                    ObjectKind::Map(_) => "[Map]".to_string(),
                    ObjectKind::Set(_) => "[Set]".to_string(),
                    ObjectKind::WeakMap(_) => "[WeakMap]".to_string(),
                    ObjectKind::WeakSet(_) => "[WeakSet]".to_string(),
                    ObjectKind::WeakRef(_) => "[WeakRef]".to_string(),
                    ObjectKind::FinalizationRegistry(_) => "[FinalizationRegistry]".to_string(),
                    ObjectKind::ShadowRealm(_) => "[ShadowRealm]".to_string(),
                    ObjectKind::MapIterator(_) => "[Map Iterator]".to_string(),
                    ObjectKind::SetIterator(_) => "[Set Iterator]".to_string(),
                    ObjectKind::Generator(_) => "[Generator]".to_string(),
                    ObjectKind::ArrayBuffer(ref info) if info.shared => {
                        "[SharedArrayBuffer]".to_string()
                    }
                    ObjectKind::ArrayBuffer(_) => "[ArrayBuffer]".to_string(),
                    ObjectKind::DataView(_) => "[DataView]".to_string(),
                    ObjectKind::TypedArray(ref info) => format!("[{}]", info.name),
                    ObjectKind::Proxy(_) => "[Proxy]".to_string(),
                    ObjectKind::Temporal(_) => "[Temporal]".to_string(),
                    ObjectKind::Array(ref ary_info) => {
                        let mut string = "[ ".to_string();

                        let mut sorted_key_val =
                            (&obj_info.property)
                                .iter()
                                .collect::<Vec<(&String, &Property)>>();
                        sorted_key_val
                            .sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                        let length = ary_info.elems.len();
                        let is_last_idx = |idx: usize| -> bool { idx == length - 1 };
                        let mut i = 0;
                        while i < length {
                            let mut empty_elems = 0;
                            while i < length && Value::empty() == ary_info.elems[i].as_data().val {
                                empty_elems += 1;
                                i += 1;
                            }

                            if empty_elems > 0 {
                                string = format!(
                                    "{}<{} empty item{}>{}",
                                    string,
                                    empty_elems,
                                    if empty_elems >= 2 { "s" } else { "" },
                                    if is_last_idx(i - 1) && sorted_key_val.len() == 0 {
                                        " "
                                    } else {
                                        ", "
                                    }
                                );

                                if is_last_idx(i - 1) {
                                    break;
                                }
                            }

                            string = format!(
                                "{}{}{}",
                                string,
                                ary_info.elems[i].as_data().val.debug_string(true),
                                if is_last_idx(i) && sorted_key_val.len() == 0 {
                                    " "
                                } else {
                                    ", "
                                }
                            );

                            i += 1;
                        }

                        format!("{}{}]", string, property_string(sorted_key_val))
                    }
                }
            }
        }
    }
}

// Utils

#[inline]
pub fn is_integer(n: f64) -> bool {
    n - n.floor() == 0.0
}

#[inline]
pub fn cstrp_to_str(p: *mut String) -> &'static str {
    unsafe { &*p }
}

fn number_to_exponential_string(n: f64) -> String {
    let formatted = format!("{:e}", n);
    let mut parts = formatted.split('e');
    let mut mantissa = parts.next().unwrap_or("").to_string();
    let exponent = parts.next().unwrap_or("0").parse::<i32>().unwrap_or(0);
    if mantissa.contains('.') {
        while mantissa.ends_with('0') {
            mantissa.pop();
        }
        if mantissa.ends_with('.') {
            mantissa.pop();
        }
    }
    if exponent >= 0 {
        format!("{}e+{}", mantissa, exponent)
    } else {
        format!("{}e{}", mantissa, exponent)
    }
}
