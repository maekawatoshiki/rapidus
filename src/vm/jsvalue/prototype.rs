#![macro_use]
use super::{function::ThisMode, value::*};
use crate::builtins;
use crate::builtins::{array, function};
use crate::gc::MemoryAllocator;
use crate::id::get_unique_id;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct ObjectPrototypes {
    pub object: Value,
    pub function: Value,
    pub string: Value,
    pub array: Value,
    pub symbol: Value,
    pub error: Value,
}

impl ObjectPrototypes {
    pub fn new(allocator: &mut MemoryAllocator) -> Self {
        let object_prototype = Value::Object(allocator.alloc(ObjectInfo {
            kind: ObjectKind::Ordinary,
            prototype: Value::null(),
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }));

        let function_prototype = {
            let function_prototype = Value::Object(allocator.alloc(ObjectInfo {
                kind: ObjectKind::Function(FunctionObjectInfo {
                    name: None,
                    kind: FunctionObjectKind::User(UserFunctionInfo {
                        id: get_unique_id(),
                        module_func_id: 0,
                        params: vec![],
                        var_names: vec![],
                        lex_names: vec![],
                        func_decls: vec![],
                        constructible: false,
                        this_mode: ThisMode::Global,
                        code: vec![],
                        exception_table: vec![],
                        outer: None,
                    }),
                }),
                prototype: object_prototype,
                property: make_property_map!(),
                sym_property: FxHashMap::default(),
            }));

            let function_prototype_call = Value::builtin_function_with_proto(
                allocator,
                function_prototype,
                "call",
                function::function_prototype_call,
            );

            let mut info = function_prototype.get_object_info();
            info.prototype = object_prototype;
            info.property = make_property_map!(call: function_prototype_call);

            function_prototype
        };

        let string_prototype = {
            let index_of = Value::builtin_function_with_proto(
                allocator,
                function_prototype,
                "indexOf",
                builtins::string::string_prototype_index_of,
            );

            let split = Value::builtin_function_with_proto(
                allocator,
                function_prototype,
                "split",
                builtins::string::string_prototype_split,
            );

            Value::Object(allocator.alloc(ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                property: make_property_map!(indexOf: index_of, split: split),
                sym_property: FxHashMap::default(),
            }))
        };

        let array_prototype = {
            let push = Value::builtin_function_with_proto(
                allocator,
                function_prototype,
                "push",
                array::array_prototype_push,
            );

            let map = Value::builtin_function_with_proto(
                allocator,
                function_prototype,
                "map",
                array::array_prototype_map,
            );

            Value::Object(allocator.alloc(ObjectInfo {
                kind: ObjectKind::Array(ArrayObjectInfo { elems: vec![] }),
                prototype: object_prototype,
                property: make_property_map!(
                    length => false, false, true : Value::Number(0.0),
                    push   => true,  false, true : push,
                    map    => true,  false, true : map
                ),
                sym_property: FxHashMap::default(),
            }))
        };

        let symbol_prototype = {
            Value::Object(allocator.alloc(ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                // TODO: https://tc39.github.io/ecma262/#sec-properties-of-the-symbol-prototype-object
                property: make_property_map!(),
                sym_property: FxHashMap::default(),
            }))
        };

        let error_prototype = {
            Value::Object(allocator.alloc(ObjectInfo {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                // TODO: https://tc39.github.io/ecma262/#sec-properties-of-the-error-prototype-object
                property: make_property_map!(),
                sym_property: FxHashMap::default(),
            }))
        };

        ObjectPrototypes {
            object: object_prototype,
            function: function_prototype,
            string: string_prototype,
            array: array_prototype,
            symbol: symbol_prototype,
            error: error_prototype,
        }
    }
}
