#![macro_use]
use super::super::super::builtin;
use super::super::super::builtins::{array, function};
use super::super::super::id::get_unique_id;
use super::value::*;
use gc::MemoryAllocator;

#[derive(Debug, Clone)]
pub struct ObjectPrototypes {
    pub object: Value2,
    pub function: Value2,
    pub string: Value2,
    pub array: Value2,
}

impl ObjectPrototypes {
    pub fn new(memory_allocator: &mut MemoryAllocator) -> Self {
        let object_prototype = Value2::Object(memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Ordinary,
            property: make_property_map!(__proto__: Value2::null()),
        }));

        let function_prototype = {
            let function_prototype = Value2::Object(memory_allocator.alloc(ObjectInfo {
                kind: ObjectKind2::Function(FunctionObjectInfo {
                    id: get_unique_id(),
                    name: None,
                    kind: FunctionObjectKind::User(UserFunctionInfo {
                        params: vec![],
                        var_names: vec![],
                        lex_names: vec![],
                        func_decls: vec![],
                        constructor: false,
                        bound_variables: 0,
                        code: vec![],
                        outer: None,
                        exception_table: vec![],
                    }),
                }),
                property: make_property_map!(__proto__: object_prototype),
            }));

            let function_prototype_call = Value2::builtin_function_with_proto(
                memory_allocator,
                function_prototype,
                "call".to_string(),
                function::function_prototype_call,
            );

            let info = function_prototype.get_object_info();
            info.property =
                make_property_map!(__proto__: object_prototype, call: function_prototype_call);

            function_prototype
        };

        let string_prototype = {
            let index_of = Value2::builtin_function_with_proto(
                memory_allocator,
                function_prototype,
                "indexOf".to_string(),
                builtin::string_prototype_index_of,
            );

            let split = Value2::builtin_function_with_proto(
                memory_allocator,
                function_prototype,
                "split".to_string(),
                builtin::string_prototype_split,
            );

            Value2::Object(memory_allocator.alloc(ObjectInfo {
                kind: ObjectKind2::Ordinary,
                property: make_property_map!(
                    __proto__: object_prototype,
                    indexOf: index_of,
                    split: split
                ),
            }))
        };

        let array_prototype = {
            let push = Value2::builtin_function_with_proto(
                memory_allocator,
                function_prototype,
                "push".to_string(),
                array::array_prototype_push,
            );

            let map = Value2::builtin_function_with_proto(
                memory_allocator,
                function_prototype,
                "map".to_string(),
                array::array_prototype_map,
            );

            Value2::Object(memory_allocator.alloc(ObjectInfo {
                property: make_property_map!(
                    __proto__ => false, false, false: object_prototype,
                    length    => false, false, true : Value2::Number(0.0),
                    push      => true,  false, true : push,
                    map       => true,  false, true : map
                ),
                kind: ObjectKind2::Array(ArrayObjectInfo { elems: vec![] }),
            }))
        };

        ObjectPrototypes {
            object: object_prototype,
            function: function_prototype,
            string: string_prototype,
            array: array_prototype,
        }
    }
}
