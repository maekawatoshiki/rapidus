use super::super::error;
use super::super::frame::LexicalEnvironmentRef;
pub use super::array::*;
pub use super::function::*;
pub use super::object::*;
pub use super::prototype::*;
pub use super::symbol::*;
use crate::builtin::BuiltinFuncTy2;
use crate::gc;
use crate::vm::vm::Factory;
pub use rustc_hash::FxHashMap;
use std::ffi::CString;

pub const UNINITIALIZED: i32 = 0;
pub const EMPTY: i32 = 1;
pub const NULL: i32 = 2;
pub const UNDEFINED: i32 = 3;

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
        String(*mut CString), // TODO: Using CString is good for JIT. However, we need better one instead.
        Object(*mut ObjectInfo),
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
        let mut record = FxHashMap::default();
        $( record.insert(
            (stringify!($property_name)).to_string(),
            Property::Data(DataProperty {
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
macro_rules! make_normal_object {
    ($factory:expr) => { {
        Value::Object($factory.alloc(
            ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: $factory.object_prototypes.object,
                property: FxHashMap::default(),
                sym_property: FxHashMap::default()
            }
        ))
    } };
    ($memory_allocator:expr, $object_prototypes:expr) => { {
        Value::Object($memory_allocator.alloc(
            ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: $object_prototypes.object,
                property: FxHashMap::default(),
                sym_property: FxHashMap::default()
            }
        ))
    } };
    ($memory_allocator:expr, $object_prototypes:expr, $($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        Value::Object($memory_allocator.alloc(
            ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: $object_prototypes.object,
                property: make_property_map_sub!($($property_name, $val, $x, $y, $z),* ),
                sym_property: FxHashMap::default()
            }
            ))
    } };
    ($factory:expr, $($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        Value::Object($factory.alloc(
            ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: $factory.object_prototypes.object,
                property: make_property_map_sub!($($property_name, $val, $x, $y, $z),* ),
                sym_property: FxHashMap::default()
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
            Value::Other(i) => write!(f, "Other({})", i),
            Value::Object(ref info) => {
                let info = ObjectRef(*info);
                match info.kind {
                    ObjectKind::Ordinary => write!(f, "Object"),
                    ObjectKind::Function(_) => write!(f, "Function"),
                    ObjectKind::Array(_) => write!(f, "Array"),
                    ObjectKind::Symbol(_) => write!(f, "Symbol"),
                }
            }
        }
    }
}

impl Value {
    pub const fn null() -> Self {
        Value::Other(NULL)
    }

    pub const fn undefined() -> Self {
        Value::Other(UNDEFINED)
    }

    pub const fn uninitialized() -> Self {
        Value::Other(UNINITIALIZED)
    }

    pub const fn empty() -> Self {
        Value::Other(EMPTY)
    }

    #[inline]
    pub fn bool(x: bool) -> Self {
        Value::Bool(if x { 1 } else { 0 })
    }

    fn string(memory_allocator: &mut gc::MemoryAllocator, body: String) -> Self {
        Value::String(memory_allocator.alloc(CString::new(body).unwrap()))
    }

    pub fn builtin_function_with_proto(
        memory_allocator: &mut gc::MemoryAllocator,
        proto: Value,
        name: impl Into<String>,
        func: BuiltinFuncTy2,
    ) -> Self {
        let name: String = name.into();
        let name_prop = Value::string(memory_allocator, name.clone());
        Value::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind::Function(FunctionObjectInfo {
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            prototype: proto,
            property: make_property_map!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            sym_property: FxHashMap::default(),
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

    pub fn get_property_by_str_key(&self, key: &str) -> Value {
        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).get_property_by_str_key(key),
            _ => Value::undefined(),
        }
    }

    pub fn get_property(
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
                    .get_property(factory, key),
            }
        }

        match self {
            Value::String(s) => {
                return string_get_property(factory, cstrp_to_str(*s), key);
            }
            Value::Other(_) => {
                return Err(error::RuntimeError::Type(format!(
                    "TypeError: Cannot read property '{}' of {}",
                    key.to_string(),
                    self.to_string()
                )));
            }
            // TODO: Number
            _ => {}
        }

        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).get_property(factory, key),
            _ => Ok(Property::new_data_simple(Value::undefined())),
        }
    }

    pub fn set_property_by_string_key(&self, key: impl Into<String>, val: Value) {
        match self {
            Value::Object(obj_info) => {
                ObjectRef(*obj_info).set_property_by_string_key(key.into(), val)
            }
            _ => {}
        }
    }

    pub fn set_property(
        &self,
        allocator: &mut gc::MemoryAllocator,
        key: Value,
        val: Value,
    ) -> Result<Option<Value>, error::RuntimeError> {
        match self {
            Value::Object(obj_info) => ObjectRef(*obj_info).set_property(allocator, key, val),
            Value::Other(_) => Err(error::RuntimeError::Type(format!(
                "TypeError: Cannot set property '{}' of {}",
                key.to_string(),
                self.to_string()
            ))),
            _ => Ok(None),
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
            Value::String(s) => {
                let s = cstrp_to_str(*s);
                if s == "Infinity" || s == "-Infinity" {
                    ::std::f64::INFINITY
                } else if s.len() == 0 {
                    0.0
                } else if s.chars().all(|c| c.is_whitespace()) {
                    0.0
                } else {
                    s.parse::<f64>().unwrap_or(::std::f64::NAN)
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
                    "Infinity".to_string()
                } else {
                    format!("{}", n)
                }
            }
            Value::Object(info) => {
                let info = ObjectRef(*info);
                match info.kind {
                    ObjectKind::Ordinary => "[object Object]".to_string(),
                    ObjectKind::Array(ref info) => info.join(None),
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
            Value::Number(num) => {
                if *num == 0f64 || num.is_nan() {
                    false
                } else {
                    true
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
                    if !val.is_object() {
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
                    ObjectKind::Ordinary => Some(self),
                    ObjectKind::Function(_) => None,
                    ObjectKind::Array(_) => None,
                    ObjectKind::Symbol(_) => Some(self), // TODO
                }
            }
            Value::String(_) => Some(self), // TODO
            _ => None,
        }
    }
}

impl Value {
    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-addition-operator-plus-runtime-semantics-evaluation
    pub fn add(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        let lprim = self.to_primitive(allocator, None);
        let rprim = val.to_primitive(allocator, None);
        match (lprim, rprim) {
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
            (x, y) => Value::Number(x.to_number(allocator) + y.to_number(allocator)),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-subtraction-operator-minus-runtime-semantics-evaluation
    pub fn sub(self, val: Value) -> Self {
        match (self, val) {
            (Value::Number(x), Value::Number(y)) => Value::Number(x - y),
            _ => Value::undefined(),
        }
    }

    pub fn mul(self, val: Value) -> Self {
        match (self, val) {
            (Value::Number(x), Value::Number(y)) => Value::Number(x * y),
            _ => Value::undefined(),
        }
    }

    pub fn div(self, val: Value) -> Self {
        match (self, val) {
            (Value::Number(x), Value::Number(y)) => Value::Number(x / y),
            _ => Value::undefined(),
        }
    }

    pub fn rem(self, val: Value) -> Self {
        match (self, val) {
            (Value::Number(x), Value::Number(y)) => Value::Number((x as i64 % y as i64) as f64),
            _ => Value::undefined(),
        }
    }

    pub fn exp(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::Number(self.to_number(allocator).powf(val.to_number(allocator)))
    }

    pub fn and(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::Number((self.to_int32(allocator) & val.to_int32(allocator)) as f64)
    }

    pub fn or(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::Number((self.to_int32(allocator) | val.to_int32(allocator)) as f64)
    }

    pub fn xor(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::Number((self.to_int32(allocator) ^ val.to_int32(allocator)) as f64)
    }

    pub fn not(self, allocator: &mut gc::MemoryAllocator) -> Self {
        Value::Number((!self.to_int32(allocator)) as f64)
    }

    /// https://tc39.github.io/ecma262/#sec-left-shift-operator
    pub fn shift_l(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::Number((self.to_int32(allocator) << (val.to_uint32(allocator) & 0x1f)) as f64)
    }

    /// https://tc39.github.io/ecma262/#sec-signed-right-shift-operator
    pub fn shift_r(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
        Value::Number((self.to_int32(allocator) >> (val.to_uint32(allocator) & 0x1f)) as f64)
    }

    /// https://tc39.github.io/ecma262/#sec-unsigned-right-shift-operator
    pub fn z_shift_r(self, allocator: &mut gc::MemoryAllocator, val: Value) -> Self {
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
    pub fn minus(self) -> Self {
        match self {
            Value::Number(n) => Value::Number(-n),
            _ => Value::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-unary-plus-operator-runtime-semantics-evaluation
    pub fn positive(self, allocator: &mut gc::MemoryAllocator) -> Self {
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
            | (Value::Bool(_), Value::Bool(_))
            | (Value::Object(_), Value::Object(_)) => true,
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
            Value::String(_) => "string",
            Value::Object(info) => {
                let info = ObjectRef(*info);
                match info.kind {
                    ObjectKind::Function(_) => "function",
                    ObjectKind::Array(_) => "object",
                    ObjectKind::Symbol(_) => "symbol",
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
                    ObjectKind::Symbol(ref info) => format!(
                        "Symbol({})",
                        info.description.as_ref().unwrap_or(&"".to_string())
                    ),
                    ObjectKind::Function(ref func_info) => {
                        if let Some(ref name) = func_info.name {
                            format!("[Function: {}]", name)
                        } else {
                            "[Function]".to_string()
                        }
                    }
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
pub fn cstrp_to_str(p: *mut CString) -> &'static str {
    unsafe { &*p }.to_str().unwrap()
}
