use super::super::frame::LexicalEnvironmentRef;
pub use super::array::*;
pub use super::function::*;
pub use super::object::*;
pub use super::prototype::*;
use builtin::BuiltinFuncTy2;
use bytecode_gen::ByteCode;
use gc;
use id::get_unique_id;
pub use rustc_hash::FxHashMap;
use std::ffi::CString;

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
        Object(*mut ObjectInfo),
        Other(u32) // UNINITIALIZED | EMPTY | NULL | UNDEFINED
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
        let mut record = FxHashMap::default();
        $( record.insert(
            (stringify!($property_name)).to_string(),
            Property2 {
                val: $val,
                writable: $writable,
                enumerable: $enumerable,
                configurable: $configurable
            }
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
    ($memory_allocator:expr) => { {
        Value2::Object($memory_allocator.alloc(
            ObjectInfo {
                kind: ObjectKind2::Ordinary,
                property: FxHashMap::default()
            }
        ))
    } };
    ($memory_allocator:expr, $($property_name:ident => $x:ident, $y:ident, $z:ident : $val:expr),*) => { {
        Value2::Object($memory_allocator.alloc(
            ObjectInfo {
                kind: ObjectKind2::Ordinary,
                property: make_property_map_sub!($($property_name, $val, $x, $y, $z),* )
            }
            ))
    } };
}

impl Value2 {
    pub const fn null() -> Self {
        Value2::Other(NULL)
    }

    pub const fn undefined() -> Self {
        Value2::Other(UNDEFINED)
    }

    pub const fn uninitialized() -> Self {
        Value2::Other(UNINITIALIZED)
    }

    pub fn bool(x: bool) -> Self {
        Value2::Bool(if x { 1 } else { 0 })
    }

    pub fn string(memory_allocator: &mut gc::MemoryAllocator, body: String) -> Self {
        Value2::String(memory_allocator.alloc(CString::new(body).unwrap()))
    }

    pub fn object(
        memory_allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        mut properties: FxHashMap<String, Property2>,
    ) -> Self {
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Ordinary,
            property: {
                properties.insert(
                    "__proto__".to_string(),
                    Property2 {
                        val: object_prototypes.object,
                        writable: false,
                        enumerable: false,
                        configurable: false,
                    },
                );
                properties
            },
        }))
    }

    pub fn builtin_function(
        memory_allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        name: String,
        func: BuiltinFuncTy2,
    ) -> Self {
        let name_prop = Value2::string(memory_allocator, name.clone());
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: get_unique_id(),
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            property: make_property_map!(
                __proto__ => false, false, false: object_prototypes.function,
                length    => false, false, true : Value2::Number(0.0),
                name      => false, false, true : name_prop
            ),
        }))
    }

    pub fn builtin_function_with_proto(
        memory_allocator: &mut gc::MemoryAllocator,
        proto: Value2,
        name: String,
        func: BuiltinFuncTy2,
    ) -> Self {
        let name_prop = Value2::string(memory_allocator, name.clone());
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: get_unique_id(),
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            property: make_property_map!(
                __proto__ => false, false, false: proto,
                length    => false, false, true : Value2::Number(0.0),
                name      => false, false, true : name_prop
            ),
        }))
    }

    pub fn function(
        memory_allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        // TODO: Too many arguments, I think.
        name: Option<String>,
        params: Vec<FunctionParameter>,
        var_names: Vec<String>,
        lex_names: Vec<String>,
        func_decls: Vec<Value2>,
        code: ByteCode,
        exception_table: Vec<Exception>,
    ) -> Self {
        let name_prop = Value2::string(memory_allocator, name.clone().unwrap_or("".to_string()));
        let prototype = make_normal_object!(memory_allocator);
        let f = Value2::Object(memory_allocator.alloc(ObjectInfo {
            property: make_property_map!(
                __proto__ => false, false, false: object_prototypes.function,
                length    => false, false, true : Value2::Number(params.len() as f64), /* TODO: rest param */
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: get_unique_id(),
                name: name,
                kind: FunctionObjectKind::User(UserFunctionInfo {
                    params,
                    var_names,
                    lex_names,
                    func_decls,
                    code,
                    exception_table,
                    outer: None
                }),
            }),
        }));
        f.get_property_by_str_key("prototype").set_constructor(f);
        f
    }

    pub fn array(
        memory_allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        elems: Vec<Property2>,
    ) -> Self {
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            property: make_property_map!(
                // __proto__ => false, false, false: object_prototypes.function,
                // length    => false, false, true : Value2::Number(params.len() as f6
            ),
            kind: ObjectKind2::Array(ArrayObjectInfo { elems }),
        }))
    }
}

impl Value2 {
    pub fn is_object(&self) -> bool {
        match self {
            Value2::Object(_) => true,
            _ => false,
        }
    }

    pub fn has_own_property(&self, key: &str) -> bool {
        match self {
            Value2::Object(obj_info) => unsafe { &**obj_info }.has_own_property(key),
            _ => false,
        }
    }

    pub fn get_object_properties(&self) -> Option<&FxHashMap<String, Property2>> {
        match self {
            Value2::Object(obj_info) => Some(&unsafe { &**obj_info }.property),
            _ => None,
        }
    }

    pub fn get_property_by_str_key(&self, key: &str) -> Value2 {
        match self {
            Value2::Object(obj_info) => unsafe { &**obj_info }.get_property_by_str_key(key),
            _ => Value2::undefined(),
        }
    }

    pub fn get_property(
        &self,
        allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        key: Value2,
    ) -> Value2 {
        let mut string_get_property = |s: &str, key: Value2| -> Value2 {
            match key {
                Value2::Number(idx) if is_integer(idx) => {
                    Value2::string(allocator, s.chars().nth(idx as usize).unwrap().to_string())
                }
                Value2::String(x) if unsafe { &*x }.to_str().unwrap() == "length" => {
                    Value2::Number(s.chars().fold(0, |x, c| x + c.len_utf16()) as f64)
                }
                key => object_prototypes.string.get_object_info().get_property(key),
            }
        };

        match self {
            Value2::String(s) => {
                return string_get_property(unsafe { &**s }.to_str().unwrap(), key)
            }
            _ => {}
        }

        match self {
            Value2::Object(obj_info) => unsafe { &**obj_info }.get_property(key),
            _ => Value2::undefined(),
        }
    }

    pub fn set_property_by_string_key(&self, key: String, val: Value2) {
        match self {
            Value2::Object(obj_info) => {
                unsafe { &mut **obj_info }.set_property_by_string_key(key, val)
            }
            _ => {}
        }
    }

    pub fn set_property(&self, key: Value2, val: Value2) {
        match self {
            Value2::Object(obj_info) => unsafe { &mut **obj_info }.set_property(key, val),
            _ => {}
        }
    }

    pub fn set_constructor(&self, val: Value2) {
        self.get_object_info().property.insert(
            "constructor".to_string(),
            Property2 {
                val,
                writable: true,
                enumerable: false,
                configurable: true,
            },
        );
    }

    pub fn set_function_outer_environment(&mut self, env: LexicalEnvironmentRef) {
        match self {
            Value2::Object(obj) => {
                let obj = unsafe { &mut **obj };
                match obj.kind {
                    ObjectKind2::Function(ref mut info) => info.set_outer_environment(env),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    pub fn copy_object(&self, memory_allocator: &mut gc::MemoryAllocator) -> Value2 {
        match self {
            Value2::Object(obj) => {
                Value2::Object(memory_allocator.alloc(unsafe { &**obj }.clone()))
            }
            e => *e,
        }
    }

    pub fn as_function(&self) -> &FunctionObjectInfo {
        match self {
            Value2::Object(obj) => {
                let obj = unsafe { &**obj };
                match obj.kind {
                    ObjectKind2::Function(ref info) => return info,
                    _ => panic!(),
                }
            }
            e => panic!("{:?}", e),
        }
    }

    pub fn get_object_info(&self) -> &mut ObjectInfo {
        match self {
            Value2::Object(obj) => unsafe { &mut **obj },
            _ => panic!(),
        }
    }

    pub fn into_number(self) -> f64 {
        match self {
            Value2::Number(x) => x,
            _ => panic!(),
        }
    }

    pub fn into_str(self) -> &'static str {
        match self {
            Value2::String(s) => unsafe { &*s }.to_str().unwrap(),
            _ => panic!(),
        }
    }

    pub fn into_bool(self) -> bool {
        match self {
            Value2::Bool(b) => {
                if b == 1 {
                    true
                } else {
                    false
                }
            }
            _ => panic!(),
        }
    }
}

impl Value2 {
    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-tonumber
    pub fn to_number(&self) -> f64 {
        match self {
            Value2::Other(UNDEFINED) => ::std::f64::NAN,
            Value2::Other(NULL) => 0.0,
            Value2::Bool(0) => 0.0,
            Value2::Bool(1) => 1.0,
            Value2::Number(n) => *n,
            Value2::String(s) => {
                let s = unsafe { &**s }.to_str().unwrap();
                if s == "Infinity" || s == "-Infinity" {
                    ::std::f64::INFINITY
                } else {
                    s.parse::<f64>().unwrap_or(::std::f64::NAN)
                }
            }
            // TODO
            _ => 0.0,
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-tostring
    pub fn to_string(&self) -> String {
        match self {
            Value2::String(s) => unsafe { &**s }.to_str().unwrap().to_string(),
            Value2::Other(UNDEFINED) => "undefined".to_string(),
            Value2::Number(n) => {
                if n.is_nan() {
                    "NaN".to_string()
                } else if n.is_infinite() {
                    "Infinity".to_string()
                } else {
                    format!("{}", n)
                }
            }
            _ => "[unimplemented]".to_string(),
        }
    }

    pub fn to_boolean(&self) -> bool {
        match self {
            Value2::Bool(0) => false,
            Value2::Bool(1) => true,
            // TODO
            _ => false,
        }
    }
}

impl Value2 {
    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-addition-operator-plus-runtime-semantics-evaluation
    pub fn add(self, memory_allocator: &mut gc::MemoryAllocator, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Number(x + y),
            (Value2::String(x), Value2::String(y)) => {
                let x = unsafe { &*x }.to_str().unwrap();
                let y = unsafe { &*y }.to_str().unwrap();
                let cat = format!("{}{}", x, y);
                Value2::string(memory_allocator, cat)
            }
            _ => Value2::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-subtraction-operator-minus-runtime-semantics-evaluation
    pub fn sub(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Number(x - y),
            _ => Value2::undefined(),
        }
    }

    pub fn mul(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Number(x * y),
            _ => Value2::undefined(),
        }
    }

    pub fn div(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Number(x / y),
            _ => Value2::undefined(),
        }
    }

    pub fn rem(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Number((x as i64 % y as i64) as f64),
            _ => Value2::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-abstract-equality-comparison
    pub fn eq(self, val: Value2) -> Self {
        if self.is_same_type_as(&val) {
            return self.strict_eq(val);
        }

        match (self, val) {
            (Value2::Other(NULL), Value2::Other(UNDEFINED)) => return Value2::bool(true),
            (Value2::Other(UNDEFINED), Value2::Other(NULL)) => return Value2::bool(true),
            _ => {}
        }

        match (self, val) {
            (Value2::Number(x), Value2::String(_)) => Value2::bool(x == val.to_number()),
            (Value2::String(_), Value2::Number(y)) => Value2::bool(self.to_number() == y),
            (Value2::Bool(_), Value2::Number(y)) => Value2::bool(self.to_number() == y),
            (Value2::Number(x), Value2::Bool(_)) => Value2::bool(x == val.to_number()),
            // (Value2::Number(x), Value2::Number(y)) => Value2::Bool(if x == y { 1 } else { 0 }),
            // (Value2::Number(_), obj) | (Value2::String(_), obj) => self.eq(val),
            _ => Value2::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-strict-equality-comparison
    pub fn strict_eq(self, val: Value2) -> Self {
        if !self.is_same_type_as(&val) {
            return Value2::bool(false);
        }

        if self == Value2::undefined() || self == Value2::null() {
            return Value2::bool(true);
        }

        match self {
            Value2::Number(_) => Value2::bool(self.into_number() == val.into_number()),
            Value2::String(_) => Value2::bool(self.into_str() == val.into_str()),
            Value2::Bool(_) => Value2::bool(self.into_bool() == val.into_bool()),
            _ => Value2::bool(false),
        }
    }

    pub fn ne(self, val: Value2) -> Self {
        Value2::bool(!self.eq(val).into_bool())
    }

    pub fn strict_ne(self, val: Value2) -> Self {
        Value2::bool(!self.strict_eq(val).into_bool())
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-abstract-relational-comparison
    pub fn lt(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Bool(if x < y { 1 } else { 0 }),
            _ => Value2::undefined(),
        }
    }

    pub fn le(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Bool(if x <= y { 1 } else { 0 }),
            _ => Value2::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-unary-minus-operator-runtime-semantics-evaluation
    pub fn minus(self) -> Self {
        match self {
            Value2::Number(n) => Value2::Number(-n),
            _ => Value2::undefined(),
        }
    }

    // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-unary-plus-operator-runtime-semantics-evaluation
    pub fn positive(self) -> Self {
        Value2::Number(self.to_number())
    }

    pub fn is_same_type_as(&self, val: &Value2) -> bool {
        match (self, val) {
            (Value2::Other(UNINITIALIZED), Value2::Other(UNINITIALIZED))
            | (Value2::Other(EMPTY), Value2::Other(EMPTY))
            | (Value2::Other(NULL), Value2::Other(NULL))
            | (Value2::Other(UNDEFINED), Value2::Other(UNDEFINED))
            | (Value2::Number(_), Value2::Number(_))
            | (Value2::String(_), Value2::String(_))
            | (Value2::Bool(_), Value2::Bool(_))
            | (Value2::Object(_), Value2::Object(_)) => true,
            _ => false,
        }
    }

    // TODO: Correct implementation: https://www.ecma-international.org/ecma-262/6.0/#sec-typeof-operator-runtime-semantics-evaluation
    pub fn type_of(&self) -> &str {
        match self {
            Value2::Other(UNDEFINED) => "undefined",
            Value2::Other(NULL) => "object",
            Value2::Bool(_) => "boolean",
            Value2::Number(_) => "number",
            Value2::String(_) => "string",
            Value2::Object(info) => {
                let info = unsafe { &**info };
                match info.kind {
                    ObjectKind2::Function(_) => "function",
                    ObjectKind2::Array(_) => "object",
                    ObjectKind2::Ordinary => "object",
                }
            }
            _ => panic!(),
        }
    }
}

impl Value2 {
    pub fn debug_string(&self, nest: bool) -> String {
        fn property_string(sorted_key_val: Vec<(&String, &Property2)>) -> String {
            sorted_key_val
                .iter()
                .enumerate()
                .fold("".to_string(), |acc, (i, tupple)| {
                    format!(
                        "{}'{}': {}{}",
                        acc,
                        tupple.0,
                        tupple.1.val.debug_string(true),
                        if i != sorted_key_val.len() - 1 {
                            ", "
                        } else {
                            " "
                        }
                    )
                })
        }

        match self {
            Value2::Other(UNINITIALIZED) => "uninitialized".to_string(),
            Value2::Other(EMPTY) => "empty".to_string(),
            Value2::Other(NULL) => "null".to_string(),
            Value2::Other(UNDEFINED) => "undefined".to_string(),
            Value2::Other(_) => unreachable!(),
            Value2::Bool(1) => "true".to_string(),
            Value2::Bool(0) => "false".to_string(),
            Value2::Bool(_) => unreachable!(),
            Value2::Number(n) => {
                if n.is_nan() {
                    "NaN".to_string()
                } else if n.is_infinite() {
                    "Infinity".to_string()
                } else {
                    format!("{}", n)
                }
            }
            Value2::String(s) => {
                let s = unsafe { &**s };
                if nest {
                    format!("'{}'", s.to_str().unwrap())
                } else {
                    s.to_str().unwrap().to_string()
                }
            }
            Value2::Object(obj_info) => {
                let obj_info = unsafe { &**obj_info };
                match obj_info.kind {
                    ObjectKind2::Ordinary => {
                        let mut sorted_key_val =
                            (&obj_info.property)
                                .iter()
                                .collect::<Vec<(&String, &Property2)>>();
                        sorted_key_val
                            .sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
                        sorted_key_val.retain(|(ref key, _)| key != &"__proto__");

                        format!("{{ {} }}", property_string(sorted_key_val))
                    }
                    ObjectKind2::Function(ref func_info) => {
                        if let Some(ref name) = func_info.name {
                            format!("[Function: {}]", name)
                        } else {
                            "[Function]".to_string()
                        }
                    }
                    ObjectKind2::Array(ref ary_info) => unimplemented!(),
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
