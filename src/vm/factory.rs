use crate::builtin::BuiltinFuncTy;
use crate::gc;
use crate::vm::{
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::value::{
        ArrayObjectInfo, ErrorObjectInfo, FunctionObjectInfo, FunctionObjectKind, ObjectInfo,
        ObjectKind, Property, SymbolInfo, UserFunctionInfo, Value,
    },
};
use rustc_hash::FxHashMap;

#[derive(Clone, Hash, Copy)]
pub struct FunctionId(usize);

impl PartialEq for FunctionId {
    fn eq(&self, other: &FunctionId) -> bool {
        self.0 == other.0
    }
}
impl Eq for FunctionId {}

impl FunctionId {
    pub fn default() -> Self {
        FunctionId(0)
    }
}

impl std::fmt::Debug for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct Factory {
    pub memory_allocator: gc::MemoryAllocator,
    pub object_prototypes: ObjectPrototypes,
    pub next_func_id: usize,
}

impl Factory {
    pub fn new(memory_allocator: gc::MemoryAllocator, object_prototypes: ObjectPrototypes) -> Self {
        Factory {
            memory_allocator,
            object_prototypes,
            next_func_id: 0,
        }
    }

    pub fn alloc<T: gc::GcTarget + 'static>(&mut self, data: T) -> *mut T {
        self.memory_allocator.alloc(data)
    }

    pub fn new_func_id(&mut self) -> FunctionId {
        let id = self.next_func_id;
        self.next_func_id = id + 1;
        FunctionId(id)
    }

    pub fn main_func_id(&mut self) -> FunctionId {
        FunctionId(0)
    }

    /// Generate Value for a string.
    pub fn string(&mut self, body: impl Into<String>) -> Value {
        Value::String(self.alloc(std::ffi::CString::new(body.into()).unwrap()))
    }

    /// Generate Value for an object.
    pub fn object(&mut self, property: FxHashMap<String, Property>) -> Value {
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Ordinary,
            prototype: self.object_prototypes.object,
            property,
            sym_property: FxHashMap::default(),
        }))
    }

    /// Generate Value for a JS function.
    pub fn function(&mut self, name: Option<String>, info: UserFunctionInfo) -> Value {
        let name_prop = self.string(name.clone().unwrap_or("".to_string()));
        let prototype = self.object(FxHashMap::default());

        let f = Value::Object(self.alloc(ObjectInfo {
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length    => false, false, true : Value::Number(info.params.len() as f64), /* TODO: rest param */
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            kind: ObjectKind::Function(FunctionObjectInfo {
                name: name,
                kind: FunctionObjectKind::User{info, outer_env: None},
            }),
            sym_property: FxHashMap::default(),
        }));

        f.get_property("prototype")
            .get_object_info()
            .property
            .insert("constructor".to_string(), Property::new_data_simple(f));

        f
    }

    /// Generate Value for a built-in (native) function.
    pub fn builtin_function(
        &mut self,
        name: impl Into<String>,
        func: crate::builtin::BuiltinFuncTy,
    ) -> Value {
        let name: String = name.into();
        let name_prop = self.string(name.clone());
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Function(FunctionObjectInfo {
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn array(&mut self, elems: Vec<Property>) -> Value {
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Array(ArrayObjectInfo { elems }),
            prototype: self.object_prototypes.array,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn symbol(&mut self, description: Option<String>) -> Value {
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Symbol(SymbolInfo {
                id: crate::id::get_unique_id(),
                description,
            }),
            prototype: self.object_prototypes.symbol,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn error(&mut self, message: impl Into<String>) -> Value {
        let message = self.string(message.into());
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Error(ErrorObjectInfo::new()),
            prototype: self.object_prototypes.error,
            property: make_property_map!(
                message => true, false, true: message
            ),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn generate_builtin_constructor(
        &mut self,
        constructor_name: impl Into<String>,
        constructor_func: BuiltinFuncTy,
        prototype: Value,
    ) -> Value {
        let ary = self.builtin_function(constructor_name, constructor_func);
        ary.set_property("prototype", prototype);
        ary.get_property("prototype").set_constructor(ary);
        ary
    }
}
