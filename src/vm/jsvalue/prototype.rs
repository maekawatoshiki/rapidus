#![macro_use]
use super::super::super::id::get_unique_id;
use super::value::*;
use gc::MemoryAllocator;

#[derive(Debug, Clone)]
pub struct ObjectPrototypes {
    pub object: Value2,
    pub function: Value2,
}

impl ObjectPrototypes {
    pub fn new(memory_allocator: &mut MemoryAllocator) -> Self {
        let object_prototype = Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Ordinary,
            property: make_property_map!(__proto__: Value2::null()),
        }));

        let function_prototype = Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                id: get_unique_id(),
                name: None,
                kind: FunctionObjectKind::User(UserFunctionInfo {
                    params: vec![],
                    var_names: vec![],
                    lex_names: vec![],
                    func_decls: vec![],
                    code: vec![],
                    outer: None,
                    exception_table: vec![],
                }),
            }),
            property: make_property_map!(__proto__: object_prototype),
        }));

        ObjectPrototypes {
            object: object_prototype,
            function: function_prototype,
        }
    }
}
