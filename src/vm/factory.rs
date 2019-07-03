use crate::builtin::BuiltinFuncTy;
use crate::gc;
use crate::vm::{
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::value::{
        ArrayObjectInfo, ErrorObjectInfo, FuncInfoRef, FunctionObjectInfo, FunctionObjectKind,
        ObjectInfo, ObjectKind, Property, SymbolInfo, UserFunctionInfo, Value,
    },
};
use rustc_hash::FxHashMap;

#[derive(Clone, Hash, Copy)]
pub struct FunctionId(pub usize);

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
    pub func_refs: Vec<Option<FuncInfoRef>>,
    pub next_func_id: usize,
}

impl Factory {
    pub fn new(memory_allocator: gc::MemoryAllocator, object_prototypes: ObjectPrototypes) -> Self {
        let mut factory = Factory {
            memory_allocator,
            object_prototypes,
            func_refs: vec![None; 30],
            next_func_id: 1,
        };
        let func_ref =
            factory.alloc_user_func_info(FunctionId::default(), UserFunctionInfo::default());
        factory.func_refs[0] = Some(func_ref);
        factory
    }

    pub fn alloc<T: gc::GcTarget + 'static>(&mut self, data: T) -> *mut T {
        self.memory_allocator.alloc(data)
    }
}

impl Factory {
    pub fn new_func_id(&mut self) -> FunctionId {
        let id = self.next_func_id;
        self.next_func_id = id + 1;
        FunctionId(id)
    }

    pub fn default_func_id(&mut self) -> FunctionId {
        FunctionId(0)
    }

    pub fn alloc_user_func_info(
        &mut self,
        func_id: FunctionId,
        user_func_info: UserFunctionInfo,
    ) -> FuncInfoRef {
        let func_ref = FuncInfoRef::new(Box::into_raw(Box::new(user_func_info)));
        let len = self.func_refs.len();
        if func_id.0 < len {
            if self.func_refs[func_id.0].is_some() {
                panic!("already exists!");
            }
            self.func_refs[func_id.0] = Some(func_ref);
        } else if func_id.0 == len {
            self.func_refs.push(Some(func_ref));
        } else {
            self.func_refs.resize(func_id.0, None);
            self.func_refs.push(Some(func_ref));
        }
        func_ref
    }

    pub fn get_func_ref(&self, func_id: FunctionId) -> FuncInfoRef {
        if func_id.0 >= self.func_refs.len() {
            panic!("FunctionId is not exists.");
        }
        if let Some(func_ref) = self.func_refs[func_id.0] {
            func_ref
        } else {
            panic!("None!");
        }
    }

    pub fn get_default_func_ref(&self) -> FuncInfoRef {
        if let Some(func_ref) = self.func_refs[0] {
            func_ref
        } else {
            unreachable!();
        }
    }

    pub fn print_func_refs(&self) {
        for i in 0..self.func_refs.len() {
            if let Some(info) = self.func_refs[i] {
                println!("  {:?}", info);
            }
        }
    }
}

impl Factory {
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
    pub fn function(&mut self, name: Option<String>, info: FuncInfoRef) -> Value {
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
