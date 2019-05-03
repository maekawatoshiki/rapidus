use builtin::BuiltinFuncTy2;
use gc::MemoryAllocator;
use rustc_hash::FxHashMap;
use vm::jsvalue::array::ArrayObjectInfo;
use vm::jsvalue::function::{FunctionObjectInfo, FunctionObjectKind, UserFunctionInfo};
use vm::jsvalue::object::{ObjectInfo, ObjectKind2, Property};
use vm::jsvalue::prototype::ObjectPrototypes;
use vm::jsvalue::symbol::SymbolInfo;
use vm::jsvalue::value::Value;

#[derive(Debug)]
pub struct Factory {
    pub memory_allocator: MemoryAllocator,
    pub object_prototypes: ObjectPrototypes,
}

impl Factory {
    pub fn string(&mut self, body: String) -> Value {
        Value::String(
            self.memory_allocator
                .alloc(std::ffi::CString::new(body).unwrap()),
        )
    }

    pub fn object(&mut self, property: FxHashMap<String, Property>) -> Value {
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Ordinary,
            prototype: self.object_prototypes.object,
            property,
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn function(&mut self, name: Option<String>, info: UserFunctionInfo) -> Value {
        let name_prop = self.string(name.clone().unwrap_or("".to_string()));
        let prototype = self.object(FxHashMap::default());

        let f = Value::Object(self.memory_allocator.alloc(ObjectInfo {
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length    => false, false, true : Value::Number(info.params.len() as f64), /* TODO: rest param */
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            kind: ObjectKind2::Function(FunctionObjectInfo {
                name: name,
                kind: FunctionObjectKind::User(info)
            }),
            sym_property: FxHashMap::default(),
        }));

        f.get_property_by_str_key("prototype")
            .get_object_info()
            .property
            .insert("constructor".to_string(), Property::new_data_simple(f));

        f
    }

    pub fn builtin_function(&mut self, name: String, func: BuiltinFuncTy2) -> Value {
        let name_prop = self.string(name.clone());
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
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
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Array(ArrayObjectInfo { elems }),
            prototype: self.object_prototypes.array,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn symbol(&mut self, description: Option<String>) -> Value {
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Symbol(SymbolInfo {
                id: ::id::get_unique_id(),
                description,
            }),
            prototype: self.object_prototypes.symbol,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }
}
