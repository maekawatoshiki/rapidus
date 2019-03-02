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
        Object(*mut ObjectInfo), // Object(FxHashMap<String, Value>),
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
        let prototype = make_normal_object!(memory_allocator);
        Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: get_unique_id(),
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            property: make_property_map!(
                __proto__ => false, false, false: object_prototypes.function,
                prototype => true , true , true : prototype,
                length    => false, false, true : Value2::Number(0.0),
                name      => false, false, true : name_prop
            ),
        }))
    }

    pub fn function(
        memory_allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        name: Option<String>,
        params: Vec<FunctionParameter>,
        var_names: Vec<String>,
        lex_names: Vec<String>,
        func_decls: Vec<Value2>,
        code: ByteCode,
    ) -> Self {
        let name_prop = Value2::string(memory_allocator, name.clone().unwrap_or("".to_string()));
        let prototype = make_normal_object!(memory_allocator);
        Value2::Object(memory_allocator.alloc(ObjectInfo {
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
                }),
            }),
        }))
    }
}

impl Value2 {
    pub fn get_property(&self, key: Value2) -> Value2 {
        match self {
            Value2::Object(obj_info) => {
                unsafe { &**obj_info }.get_property(key.to_string().as_str())
            }
            _ => Value2::undefined(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value2::String(s) => unsafe { &**s }.to_str().unwrap().to_string(),
            _ => "[unimplemented]".to_string(),
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

    pub fn get_object_info(&self) -> &mut ObjectInfo {
        match self {
            Value2::Object(obj) => unsafe { &mut **obj },
            _ => panic!(),
        }
    }
}

impl Value2 {
    // TODO:
    // https://www.ecma-international.org/ecma-262/6.0/#sec-addition-operator-plus-runtime-semantics-evaluation
    pub fn add(self, val: Value2) -> Self {
        match (self, val) {
            (Value2::Number(x), Value2::Number(y)) => Value2::Number(x + y),
            _ => Value2::undefined(),
        }
    }
}
