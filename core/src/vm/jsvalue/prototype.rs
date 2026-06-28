#![macro_use]
use super::value::Value;
use super::value::*;
use crate::builtins;
use crate::builtins::{array, date, function, object};
use crate::vm::jsvalue::symbol::{
    SYMBOL_ITERATOR_ID, SYMBOL_TO_PRIMITIVE_ID, SYMBOL_TO_STRING_TAG_ID, SYMBOL_UNSCOPABLES_ID,
};
use crate::vm::vm::Factory;
use rustc_hash::FxHashMap;

fn builtin_function_with_length(
    factory: &mut Factory,
    function_prototype: Value,
    name: &str,
    func: builtins::BuiltinFuncTy,
    length: f64,
) -> Value {
    let function = Value::builtin_function_with_proto(
        &mut factory.memory_allocator,
        function_prototype,
        name,
        func,
    );
    function.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );
    function
}

#[derive(Debug, Clone)]
pub struct ObjectPrototypes {
    pub object: Value,
    pub function: Value,
    pub string: Value,
    pub iterator: Value,
    pub generator: Value,
    pub async_generator: Value,
    pub string_iterator: Value,
    pub number: Value,
    pub bigint: Value,
    pub boolean: Value,
    pub array: Value,
    pub regexp: Value,
    pub array_buffer: Value,
    pub shared_array_buffer: Value,
    pub data_view: Value,
    pub array_iterator: Value,
    pub map: Value,
    pub map_iterator: Value,
    pub set: Value,
    pub set_iterator: Value,
    pub weak_map: Value,
    pub weak_set: Value,
    pub date: Value,
    pub symbol: Value,
    pub error: Value,
    pub aggregate_error: Value,
    pub eval_error: Value,
    pub range_error: Value,
    pub reference_error: Value,
    pub syntax_error: Value,
    pub type_error: Value,
    pub uri_error: Value,
}

impl ObjectPrototypes {
    pub fn new(factory: &mut Factory) -> Self {
        let object_prototype = Value::Object(factory.alloc(Object {
            kind: ObjectKind::Ordinary,
            prototype: Value::null(),
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }));
        let default_func_ref = factory.get_default_func_ref();
        // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-properties-of-the-function-prototype-object
        let function_prototype = {
            let function_prototype = Value::Object(factory.alloc(Object {
                kind: ObjectKind::Function(FunctionObjectInfo {
                    name: None,
                    super_constructor: None,
                    kind: FunctionObjectKind::User {
                        info: default_func_ref,
                        outer_env: None,
                    },
                }),
                prototype: object_prototype,
                property: make_property_map!(),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true,
            }));

            let function_prototype_call = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "call",
                function::function_prototype_call,
            );
            function_prototype_call.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
            let function_prototype_apply = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "apply",
                function::function_prototype_apply,
            );
            function_prototype_apply.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );
            let function_prototype_bind = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "bind",
                function::function_prototype_bind,
            );
            function_prototype_bind.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
            let function_prototype_to_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toString",
                function::function_prototype_to_string,
            );
            function_prototype_to_string
                .get_object_info()
                .property
                .insert(
                    "length".to_string(),
                    Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
                );
            let mut info = function_prototype.get_object_info();
            info.prototype = object_prototype;
            info.property = make_property_map!(
                length => false, false, true: Value::Number(0.0),
                call => true, false, true: function_prototype_call,
                apply => true, false, true: function_prototype_apply,
                bind => true, false, true: function_prototype_bind,
                toString => true, false, true: function_prototype_to_string
            );
            info.property_order = make_property_order!(
                length => false, false, true: Value::Number(0.0),
                call => true, false, true: function_prototype_call,
                apply => true, false, true: function_prototype_apply,
                bind => true, false, true: function_prototype_bind,
                toString => true, false, true: function_prototype_to_string
            );

            function_prototype
        };

        let iterator_prototype =
            builtins::iterator::iterator_prototype(factory, function_prototype, object_prototype);
        let generator_prototype = builtins::generator::generator_prototype(
            factory,
            function_prototype,
            iterator_prototype,
        );
        let async_generator_prototype = builtins::generator::async_generator_prototype(
            factory,
            function_prototype,
            iterator_prototype,
        );

        let array_iterator_prototype = {
            let next = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "next",
                array::array_iterator_next,
            );

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: iterator_prototype,
                property: make_property_map!(next => true, false, true : next),
                property_order: make_property_order!(next => true, false, true : next),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let string_iterator_prototype = {
            let next = builtin_function_with_length(
                factory,
                function_prototype,
                "next",
                builtins::string::string_iterator_next,
                0.0,
            );
            let tag = factory.string("String Iterator");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: iterator_prototype,
                property: make_property_map!(next => true, false, true : next),
                property_order: make_property_order!(next => true, false, true : next),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let map_iterator_prototype = {
            let next = builtin_function_with_length(
                factory,
                function_prototype,
                "next",
                builtins::collection::map_iterator_next,
                0.0,
            );
            let iterator = builtin_function_with_length(
                factory,
                function_prototype,
                "[Symbol.iterator]",
                builtins::collection::iterator_identity,
                0.0,
            );
            let map_iterator_tag = factory.string("Map Iterator");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: iterator_prototype,
                property: make_property_map!(next => true, false, true : next),
                property_order: make_property_order!(next => true, false, true : next),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_ITERATOR_ID,
                        Property::new_data(
                            DataProperty::new(iterator)
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(map_iterator_tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let set_iterator_prototype = {
            let next = builtin_function_with_length(
                factory,
                function_prototype,
                "next",
                builtins::collection::set_iterator_next,
                0.0,
            );
            let iterator = builtin_function_with_length(
                factory,
                function_prototype,
                "[Symbol.iterator]",
                builtins::collection::iterator_identity,
                0.0,
            );
            let set_iterator_tag = factory.string("Set Iterator");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: iterator_prototype,
                property: make_property_map!(next => true, false, true : next),
                property_order: make_property_order!(next => true, false, true : next),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_ITERATOR_ID,
                        Property::new_data(
                            DataProperty::new(iterator)
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(set_iterator_tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        {
            let has_own_property = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "hasOwnProperty",
                object::object_prototype_has_own_property,
            );
            let property_is_enumerable = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "propertyIsEnumerable",
                object::object_prototype_property_is_enumerable,
            );
            let is_prototype_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "isPrototypeOf",
                object::object_prototype_is_prototype_of,
            );
            let to_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toString",
                object::object_prototype_to_string,
            );
            let to_locale_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toLocaleString",
                object::object_prototype_to_string,
            );
            let value_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "valueOf",
                object::object_prototype_value_of,
            );
            let define_getter = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "__defineGetter__",
                object::object_prototype_define_getter,
            );
            let define_setter = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "__defineSetter__",
                object::object_prototype_define_setter,
            );
            let lookup_getter = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "__lookupGetter__",
                object::object_prototype_lookup_getter,
            );
            let lookup_setter = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "__lookupSetter__",
                object::object_prototype_lookup_setter,
            );
            let proto_get = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "get __proto__",
                object::object_prototype_proto_get,
            );
            let proto_set = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "set __proto__",
                object::object_prototype_proto_set,
            );
            define_getter.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );
            define_setter.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );
            lookup_getter.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
            lookup_setter.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
            proto_get.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );
            proto_set.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
            for function in [has_own_property, property_is_enumerable, is_prototype_of] {
                function.get_object_info().property.insert(
                    "length".to_string(),
                    Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
                );
            }
            let mut info = object_prototype.get_object_info();
            info.property.insert(
                "hasOwnProperty".to_string(),
                Property::new_data(
                    DataProperty::new(has_own_property)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "isPrototypeOf".to_string(),
                Property::new_data(
                    DataProperty::new(is_prototype_of)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "toString".to_string(),
                Property::new_data(
                    DataProperty::new(to_string)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "toLocaleString".to_string(),
                Property::new_data(
                    DataProperty::new(to_locale_string)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "valueOf".to_string(),
                Property::new_data(
                    DataProperty::new(value_of)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "propertyIsEnumerable".to_string(),
                Property::new_data(
                    DataProperty::new(property_is_enumerable)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "__defineGetter__".to_string(),
                Property::new_data(
                    DataProperty::new(define_getter)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "__defineSetter__".to_string(),
                Property::new_data(
                    DataProperty::new(define_setter)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "__lookupGetter__".to_string(),
                Property::new_data(
                    DataProperty::new(lookup_getter)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "__lookupSetter__".to_string(),
                Property::new_data(
                    DataProperty::new(lookup_setter)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            info.property.insert(
                "__proto__".to_string(),
                Property::Accessor(AccessorProperty {
                    get: proto_get,
                    set: proto_set,
                    enumerable: false,
                    configurable: true,
                }),
            );
        }

        let string_prototype = {
            let char_at = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "charAt",
                builtins::string::string_prototype_char_at,
            );
            char_at.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let char_code_at = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "charCodeAt",
                builtins::string::string_prototype_char_code_at,
            );
            char_code_at.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let code_point_at = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "codePointAt",
                builtins::string::string_prototype_code_point_at,
            );
            code_point_at.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let concat = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "concat",
                builtins::string::string_prototype_concat,
            );
            concat.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let substring = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "substring",
                builtins::string::string_prototype_substring,
            );
            substring.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let includes = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "includes",
                builtins::string::string_prototype_includes,
            );
            includes.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let starts_with = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "startsWith",
                builtins::string::string_prototype_starts_with,
            );
            starts_with.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let ends_with = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "endsWith",
                builtins::string::string_prototype_ends_with,
            );
            ends_with.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let last_index_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "lastIndexOf",
                builtins::string::string_prototype_last_index_of,
            );
            last_index_of.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let slice = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "slice",
                builtins::string::string_prototype_slice,
            );
            slice.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let at = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "at",
                builtins::string::string_prototype_at,
            );
            at.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let repeat = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "repeat",
                builtins::string::string_prototype_repeat,
            );
            repeat.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let pad_start = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "padStart",
                builtins::string::string_prototype_pad_start,
            );
            pad_start.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let pad_end = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "padEnd",
                builtins::string::string_prototype_pad_end,
            );
            pad_end.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let trim = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "trim",
                builtins::string::string_prototype_trim,
            );
            trim.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let trim_start = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "trimStart",
                builtins::string::string_prototype_trim_start,
            );
            trim_start.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let trim_end = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "trimEnd",
                builtins::string::string_prototype_trim_end,
            );
            trim_end.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_lower_case = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toLowerCase",
                builtins::string::string_prototype_to_lower_case,
            );
            to_lower_case.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_locale_lower_case = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toLocaleLowerCase",
                builtins::string::string_prototype_to_lower_case,
            );
            to_locale_lower_case.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_upper_case = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toUpperCase",
                builtins::string::string_prototype_to_upper_case,
            );
            to_upper_case.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_locale_upper_case = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toLocaleUpperCase",
                builtins::string::string_prototype_to_upper_case,
            );
            to_locale_upper_case.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let is_well_formed = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "isWellFormed",
                builtins::string::string_prototype_is_well_formed,
            );
            is_well_formed.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_well_formed = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toWellFormed",
                builtins::string::string_prototype_to_well_formed,
            );
            to_well_formed.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let locale_compare = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "localeCompare",
                builtins::string::string_prototype_locale_compare,
            );
            locale_compare.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let normalize = builtin_function_with_length(
                factory,
                function_prototype,
                "normalize",
                builtins::string::string_prototype_normalize,
                0.0,
            );
            let match_ = builtin_function_with_length(
                factory,
                function_prototype,
                "match",
                builtins::string::string_prototype_match,
                1.0,
            );
            let match_all = builtin_function_with_length(
                factory,
                function_prototype,
                "matchAll",
                builtins::string::string_prototype_match_all,
                1.0,
            );
            let search = builtin_function_with_length(
                factory,
                function_prototype,
                "search",
                builtins::string::string_prototype_search,
                1.0,
            );
            let replace = builtin_function_with_length(
                factory,
                function_prototype,
                "replace",
                builtins::string::string_prototype_replace,
                2.0,
            );
            let replace_all = builtin_function_with_length(
                factory,
                function_prototype,
                "replaceAll",
                builtins::string::string_prototype_replace_all,
                2.0,
            );

            let index_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "indexOf",
                builtins::string::string_prototype_index_of,
            );
            index_of.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let split = builtin_function_with_length(
                factory,
                function_prototype,
                "split",
                builtins::string::string_prototype_split,
                2.0,
            );

            let anchor = builtin_function_with_length(
                factory,
                function_prototype,
                "anchor",
                builtins::string::string_prototype_anchor,
                1.0,
            );
            let big = builtin_function_with_length(
                factory,
                function_prototype,
                "big",
                builtins::string::string_prototype_big,
                0.0,
            );
            let blink = builtin_function_with_length(
                factory,
                function_prototype,
                "blink",
                builtins::string::string_prototype_blink,
                0.0,
            );
            let bold = builtin_function_with_length(
                factory,
                function_prototype,
                "bold",
                builtins::string::string_prototype_bold,
                0.0,
            );
            let fixed = builtin_function_with_length(
                factory,
                function_prototype,
                "fixed",
                builtins::string::string_prototype_fixed,
                0.0,
            );
            let fontcolor = builtin_function_with_length(
                factory,
                function_prototype,
                "fontcolor",
                builtins::string::string_prototype_fontcolor,
                1.0,
            );
            let fontsize = builtin_function_with_length(
                factory,
                function_prototype,
                "fontsize",
                builtins::string::string_prototype_fontsize,
                1.0,
            );
            let italics = builtin_function_with_length(
                factory,
                function_prototype,
                "italics",
                builtins::string::string_prototype_italics,
                0.0,
            );
            let link = builtin_function_with_length(
                factory,
                function_prototype,
                "link",
                builtins::string::string_prototype_link,
                1.0,
            );
            let small = builtin_function_with_length(
                factory,
                function_prototype,
                "small",
                builtins::string::string_prototype_small,
                0.0,
            );
            let strike = builtin_function_with_length(
                factory,
                function_prototype,
                "strike",
                builtins::string::string_prototype_strike,
                0.0,
            );
            let sub = builtin_function_with_length(
                factory,
                function_prototype,
                "sub",
                builtins::string::string_prototype_sub,
                0.0,
            );
            let sup = builtin_function_with_length(
                factory,
                function_prototype,
                "sup",
                builtins::string::string_prototype_sup,
                0.0,
            );
            let substr = builtin_function_with_length(
                factory,
                function_prototype,
                "substr",
                builtins::string::string_prototype_substr,
                2.0,
            );

            let to_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toString",
                builtins::string::string_prototype_to_string,
            );

            let value_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "valueOf",
                builtins::string::string_prototype_value_of,
            );
            let iterator = builtin_function_with_length(
                factory,
                function_prototype,
                "[Symbol.iterator]",
                builtins::string::string_prototype_iterator,
                0.0,
            );
            let iterator_symbol = factory.well_known_symbol(SYMBOL_ITERATOR_ID);

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                property: {
                    let mut property = make_property_map!(
                        charAt => true, false, true: char_at,
                        charCodeAt => true, false, true: char_code_at,
                        codePointAt => true, false, true: code_point_at,
                        concat => true, false, true: concat,
                        substring => true, false, true: substring,
                        includes => true, false, true: includes,
                        startsWith => true, false, true: starts_with,
                        endsWith => true, false, true: ends_with,
                        lastIndexOf => true, false, true: last_index_of,
                        slice => true, false, true: slice,
                        at => true, false, true: at,
                        repeat => true, false, true: repeat,
                        padStart => true, false, true: pad_start,
                        padEnd => true, false, true: pad_end,
                        trim => true, false, true: trim,
                        trimStart => true, false, true: trim_start,
                        trimLeft => true, false, true: trim_start,
                        trimEnd => true, false, true: trim_end,
                        trimRight => true, false, true: trim_end,
                        toLowerCase => true, false, true: to_lower_case,
                        toLocaleLowerCase => true, false, true: to_locale_lower_case,
                        toUpperCase => true, false, true: to_upper_case,
                        toLocaleUpperCase => true, false, true: to_locale_upper_case,
                        isWellFormed => true, false, true: is_well_formed,
                        toWellFormed => true, false, true: to_well_formed,
                        localeCompare => true, false, true: locale_compare,
                        normalize => true, false, true: normalize,
                        search => true, false, true: search,
                        replace => true, false, true: replace,
                        replaceAll => true, false, true: replace_all,
                        indexOf => true, false, true: index_of,
                        split => true, false, true: split,
                        anchor => true, false, true: anchor,
                        big => true, false, true: big,
                        blink => true, false, true: blink,
                        bold => true, false, true: bold,
                        fixed => true, false, true: fixed,
                        fontcolor => true, false, true: fontcolor,
                        fontsize => true, false, true: fontsize,
                        italics => true, false, true: italics,
                        link => true, false, true: link,
                        small => true, false, true: small,
                        strike => true, false, true: strike,
                        sub => true, false, true: sub,
                        sup => true, false, true: sup,
                        substr => true, false, true: substr,
                        toString => true, false, true: to_string,
                        valueOf => true, false, true: value_of
                    );
                    property.insert(
                        "match".to_string(),
                        Property::new_data(
                            DataProperty::new(match_).set_writable().set_configurable(),
                        ),
                    );
                    property.insert(
                        "matchAll".to_string(),
                        Property::new_data(
                            DataProperty::new(match_all)
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    property
                },
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_ITERATOR_ID,
                        Property::new_data(
                            DataProperty::new(iterator)
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    property
                },
                sym_property_order: vec![iterator_symbol],
                extensible: true,
            }))
        };

        let map_prototype = {
            let get = builtin_function_with_length(
                factory,
                function_prototype,
                "get",
                builtins::collection::map_prototype_get,
                1.0,
            );
            let set = builtin_function_with_length(
                factory,
                function_prototype,
                "set",
                builtins::collection::map_prototype_set,
                2.0,
            );
            let has = builtin_function_with_length(
                factory,
                function_prototype,
                "has",
                builtins::collection::map_prototype_has,
                1.0,
            );
            let delete = builtin_function_with_length(
                factory,
                function_prototype,
                "delete",
                builtins::collection::map_prototype_delete,
                1.0,
            );
            let clear = builtin_function_with_length(
                factory,
                function_prototype,
                "clear",
                builtins::collection::map_prototype_clear,
                0.0,
            );
            let keys = builtin_function_with_length(
                factory,
                function_prototype,
                "keys",
                builtins::collection::map_prototype_keys,
                0.0,
            );
            let values = builtin_function_with_length(
                factory,
                function_prototype,
                "values",
                builtins::collection::map_prototype_values,
                0.0,
            );
            let entries = builtin_function_with_length(
                factory,
                function_prototype,
                "entries",
                builtins::collection::map_prototype_entries,
                0.0,
            );
            let for_each = builtin_function_with_length(
                factory,
                function_prototype,
                "forEach",
                builtins::collection::map_prototype_for_each,
                1.0,
            );
            let size = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "get size",
                builtins::collection::map_prototype_size,
            );
            let map_tag = factory.string("Map");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                property: {
                    let mut property = make_property_map!(
                        get => true, false, true: get,
                        set => true, false, true: set,
                        has => true, false, true: has,
                        delete => true, false, true: delete,
                        clear => true, false, true: clear,
                        keys => true, false, true: keys,
                        values => true, false, true: values,
                        entries => true, false, true: entries,
                        forEach => true, false, true: for_each
                    );
                    property.insert(
                        "size".to_string(),
                        Property::Accessor(AccessorProperty {
                            get: size,
                            set: Value::undefined(),
                            enumerable: false,
                            configurable: true,
                        }),
                    );
                    property
                },
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_ITERATOR_ID,
                        Property::new_data(
                            DataProperty::new(entries).set_writable().set_configurable(),
                        ),
                    );
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(map_tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let set_prototype = {
            let add = builtin_function_with_length(
                factory,
                function_prototype,
                "add",
                builtins::collection::set_prototype_add,
                1.0,
            );
            let has = builtin_function_with_length(
                factory,
                function_prototype,
                "has",
                builtins::collection::set_prototype_has,
                1.0,
            );
            let delete = builtin_function_with_length(
                factory,
                function_prototype,
                "delete",
                builtins::collection::set_prototype_delete,
                1.0,
            );
            let clear = builtin_function_with_length(
                factory,
                function_prototype,
                "clear",
                builtins::collection::set_prototype_clear,
                0.0,
            );
            let values = builtin_function_with_length(
                factory,
                function_prototype,
                "values",
                builtins::collection::set_prototype_values,
                0.0,
            );
            let entries = builtin_function_with_length(
                factory,
                function_prototype,
                "entries",
                builtins::collection::set_prototype_entries,
                0.0,
            );
            let for_each = builtin_function_with_length(
                factory,
                function_prototype,
                "forEach",
                builtins::collection::set_prototype_for_each,
                1.0,
            );
            let size = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "get size",
                builtins::collection::set_prototype_size,
            );
            let set_tag = factory.string("Set");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                property: {
                    let mut property = make_property_map!(
                        add => true, false, true: add,
                        has => true, false, true: has,
                        delete => true, false, true: delete,
                        clear => true, false, true: clear,
                        values => true, false, true: values,
                        keys => true, false, true: values,
                        entries => true, false, true: entries,
                        forEach => true, false, true: for_each
                    );
                    property.insert(
                        "size".to_string(),
                        Property::Accessor(AccessorProperty {
                            get: size,
                            set: Value::undefined(),
                            enumerable: false,
                            configurable: true,
                        }),
                    );
                    property
                },
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_ITERATOR_ID,
                        Property::new_data(
                            DataProperty::new(values).set_writable().set_configurable(),
                        ),
                    );
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(set_tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let weak_map_prototype = {
            let get = builtin_function_with_length(
                factory,
                function_prototype,
                "get",
                builtins::collection::weak_map_prototype_get,
                1.0,
            );
            let set = builtin_function_with_length(
                factory,
                function_prototype,
                "set",
                builtins::collection::weak_map_prototype_set,
                2.0,
            );
            let has = builtin_function_with_length(
                factory,
                function_prototype,
                "has",
                builtins::collection::weak_map_prototype_has,
                1.0,
            );
            let delete = builtin_function_with_length(
                factory,
                function_prototype,
                "delete",
                builtins::collection::weak_map_prototype_delete,
                1.0,
            );
            let tag = factory.string("WeakMap");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                property: make_property_map!(
                    get => true, false, true: get,
                    set => true, false, true: set,
                    has => true, false, true: has,
                    delete => true, false, true: delete
                ),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let weak_set_prototype = {
            let add = builtin_function_with_length(
                factory,
                function_prototype,
                "add",
                builtins::collection::weak_set_prototype_add,
                1.0,
            );
            let has = builtin_function_with_length(
                factory,
                function_prototype,
                "has",
                builtins::collection::weak_set_prototype_has,
                1.0,
            );
            let delete = builtin_function_with_length(
                factory,
                function_prototype,
                "delete",
                builtins::collection::weak_set_prototype_delete,
                1.0,
            );
            let tag = factory.string("WeakSet");

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                property: make_property_map!(
                    add => true, false, true: add,
                    has => true, false, true: has,
                    delete => true, false, true: delete
                ),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let array_prototype = {
            let push = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "push",
                array::array_prototype_push,
            );
            push.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let pop = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "pop",
                array::array_prototype_pop,
            );
            pop.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let map = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "map",
                array::array_prototype_map,
            );
            map.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let concat = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "concat",
                array::array_prototype_concat,
            );
            concat.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let copy_within = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "copyWithin",
                array::array_prototype_copy_within,
            );
            copy_within.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let every = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "every",
                array::array_prototype_every,
            );
            every.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let some = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "some",
                array::array_prototype_some,
            );
            some.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let fill = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "fill",
                array::array_prototype_fill,
            );
            fill.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let includes = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "includes",
                array::array_prototype_includes,
            );
            includes.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let array_index_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "indexOf",
                array::array_prototype_index_of,
            );
            array_index_of.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let last_index_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "lastIndexOf",
                array::array_prototype_last_index_of,
            );
            last_index_of.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let filter = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "filter",
                array::array_prototype_filter,
            );
            filter.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let for_each = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "forEach",
                array::array_prototype_for_each,
            );
            for_each.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let reduce = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "reduce",
                array::array_prototype_reduce,
            );
            reduce.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let reduce_right = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "reduceRight",
                array::array_prototype_reduce_right,
            );
            reduce_right.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let find = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "find",
                array::array_prototype_find,
            );
            find.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let find_index = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "findIndex",
                array::array_prototype_find_index,
            );
            find_index.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let find_last = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "findLast",
                array::array_prototype_find_last,
            );
            find_last.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let find_last_index = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "findLastIndex",
                array::array_prototype_find_last_index,
            );
            find_last_index.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let flat = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "flat",
                array::array_prototype_flat,
            );
            let flat_map = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "flatMap",
                array::array_prototype_flat_map,
            );
            flat_map.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let join = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "join",
                array::array_prototype_join,
            );
            join.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let slice = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "slice",
                array::array_prototype_slice,
            );
            slice.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let reverse = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "reverse",
                array::array_prototype_reverse,
            );
            reverse.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let shift = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "shift",
                array::array_prototype_shift,
            );
            shift.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let unshift = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "unshift",
                array::array_prototype_unshift,
            );
            unshift.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let splice = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "splice",
                array::array_prototype_splice,
            );
            splice.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let sort = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "sort",
                array::array_prototype_sort,
            );
            sort.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let to_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toString",
                array::array_prototype_to_string,
            );
            to_string.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );
            let to_locale_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toLocaleString",
                array::array_prototype_to_locale_string,
            );
            to_locale_string.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_reversed = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toReversed",
                array::array_prototype_to_reversed,
            );
            to_reversed.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
            );

            let to_sorted = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toSorted",
                array::array_prototype_to_sorted,
            );
            to_sorted.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let to_spliced = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toSpliced",
                array::array_prototype_to_spliced,
            );
            to_spliced.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let with = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "with",
                array::array_prototype_with,
            );
            with.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
            );

            let at = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "at",
                array::array_prototype_at,
            );
            at.get_object_info().property.insert(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );

            let entries = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "entries",
                array::array_prototype_entries,
            );
            let keys = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "keys",
                array::array_prototype_keys,
            );
            let values = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "values",
                array::array_prototype_values,
            );
            let unscopables = Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: Value::null(),
                property: make_property_map!(
                    copyWithin => true, true, true : Value::bool(true),
                    entries => true, true, true : Value::bool(true),
                    fill => true, true, true : Value::bool(true),
                    find => true, true, true : Value::bool(true),
                    findIndex => true, true, true : Value::bool(true),
                    flat => true, true, true : Value::bool(true),
                    flatMap => true, true, true : Value::bool(true),
                    includes => true, true, true : Value::bool(true),
                    keys => true, true, true : Value::bool(true),
                    values => true, true, true : Value::bool(true),
                    findLast => true, true, true : Value::bool(true),
                    findLastIndex => true, true, true : Value::bool(true),
                    toReversed => true, true, true : Value::bool(true),
                    toSorted => true, true, true : Value::bool(true),
                    toSpliced => true, true, true : Value::bool(true)
                ),
                property_order: vec![
                    "copyWithin".to_string(),
                    "entries".to_string(),
                    "fill".to_string(),
                    "find".to_string(),
                    "findIndex".to_string(),
                    "flat".to_string(),
                    "flatMap".to_string(),
                    "includes".to_string(),
                    "keys".to_string(),
                    "values".to_string(),
                    "findLast".to_string(),
                    "findLastIndex".to_string(),
                    "toReversed".to_string(),
                    "toSorted".to_string(),
                    "toSpliced".to_string(),
                ],
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true,
            }));
            let mut sym_property = FxHashMap::default();
            sym_property.insert(
                SYMBOL_ITERATOR_ID,
                Property::new_data(DataProperty::new(values).set_writable().set_configurable()),
            );
            sym_property.insert(
                SYMBOL_UNSCOPABLES_ID,
                Property::new_data(DataProperty::new(unscopables).set_configurable()),
            );
            let iterator_symbol = factory.well_known_symbol(SYMBOL_ITERATOR_ID);
            let unscopables_symbol = factory.well_known_symbol(SYMBOL_UNSCOPABLES_ID);

            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Array(ArrayObjectInfo {
                    elems: vec![],
                    length: 0,
                    length_writable: true,
                }),
                prototype: object_prototype,
                property: make_property_map!(
                    length => false, false, true : Value::Number(0.0),
                    at     => true,  false, true : at,
                    entries => true, false, true : entries,
                    keys   => true,  false, true : keys,
                    values => true,  false, true : values,
                    copyWithin => true, false, true : copy_within,
                    join   => true,  false, true : join,
                    slice  => true,  false, true : slice,
                    reverse => true, false, true : reverse,
                    shift  => true,  false, true : shift,
                    unshift => true, false, true : unshift,
                    splice => true,  false, true : splice,
                    sort   => true,  false, true : sort,
                    toString => true, false, true : to_string,
                    toLocaleString => true, false, true : to_locale_string,
                    toReversed => true, false, true : to_reversed,
                    toSorted => true, false, true : to_sorted,
                    toSpliced => true, false, true : to_spliced,
                    with => true, false, true : with,
                    push   => true,  false, true : push,
                    pop    => true,  false, true : pop,
                    concat => true,  false, true : concat,
                    map    => true,  false, true : map,
                    every  => true,  false, true : every,
                    some   => true,  false, true : some,
                    fill   => true,  false, true : fill,
                    includes => true, false, true : includes,
                    indexOf => true, false, true : array_index_of,
                    lastIndexOf => true, false, true : last_index_of,
                    filter => true,  false, true : filter,
                    forEach => true, false, true : for_each,
                    reduce => true,  false, true : reduce,
                    reduceRight => true, false, true : reduce_right,
                    find   => true,  false, true : find,
                    findIndex => true, false, true : find_index,
                    findLast => true, false, true : find_last,
                    findLastIndex => true, false, true : find_last_index,
                    flat   => true,  false, true : flat,
                    flatMap => true, false, true : flat_map
                ),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property,
                sym_property_order: vec![iterator_symbol, unscopables_symbol],
                extensible: true,
            }))
        };

        let get_hours = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getHours",
            date::date_get_hours,
        );

        let get_minutes = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getMinutes",
            date::date_get_minutes,
        );
        let get_time = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getTime",
            date::date_get_time,
        );
        let value_of = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "valueOf",
            date::date_value_of,
        );
        let get_full_year = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getFullYear",
            date::date_get_full_year,
        );
        let get_utc_full_year = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCFullYear",
            date::date_get_utc_full_year,
        );
        let get_month = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getMonth",
            date::date_get_month,
        );
        let get_utc_month = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCMonth",
            date::date_get_utc_month,
        );
        let get_date = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getDate",
            date::date_get_date,
        );
        let get_utc_date = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCDate",
            date::date_get_utc_date,
        );
        let get_day = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getDay",
            date::date_get_day,
        );
        let get_utc_day = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCDay",
            date::date_get_utc_day,
        );
        let get_utc_hours = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCHours",
            date::date_get_utc_hours,
        );
        let get_utc_minutes = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCMinutes",
            date::date_get_utc_minutes,
        );
        let get_seconds = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getSeconds",
            date::date_get_seconds,
        );
        let get_utc_seconds = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCSeconds",
            date::date_get_utc_seconds,
        );
        let get_milliseconds = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getMilliseconds",
            date::date_get_milliseconds,
        );
        let get_utc_milliseconds = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getUTCMilliseconds",
            date::date_get_utc_milliseconds,
        );
        let get_timezone_offset = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getTimezoneOffset",
            date::date_get_timezone_offset,
        );
        let get_year = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "getYear",
            date::date_get_year,
        );
        let set_time = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "setTime",
            date::date_set_time,
        );
        set_time.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
        );
        let set_year = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "setYear",
            date::date_set_year,
        );
        set_year.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
        );
        let set_milliseconds = builtin_function_with_length(
            factory,
            function_prototype,
            "setMilliseconds",
            date::date_set_milliseconds,
            1.0,
        );
        let set_utc_milliseconds = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCMilliseconds",
            date::date_set_utc_milliseconds,
            1.0,
        );
        let set_seconds = builtin_function_with_length(
            factory,
            function_prototype,
            "setSeconds",
            date::date_set_seconds,
            2.0,
        );
        let set_utc_seconds = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCSeconds",
            date::date_set_utc_seconds,
            2.0,
        );
        let set_minutes = builtin_function_with_length(
            factory,
            function_prototype,
            "setMinutes",
            date::date_set_minutes,
            3.0,
        );
        let set_utc_minutes = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCMinutes",
            date::date_set_utc_minutes,
            3.0,
        );
        let set_hours = builtin_function_with_length(
            factory,
            function_prototype,
            "setHours",
            date::date_set_hours,
            4.0,
        );
        let set_utc_hours = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCHours",
            date::date_set_utc_hours,
            4.0,
        );
        let set_date = builtin_function_with_length(
            factory,
            function_prototype,
            "setDate",
            date::date_set_date,
            1.0,
        );
        let set_utc_date = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCDate",
            date::date_set_utc_date,
            1.0,
        );
        let set_month = builtin_function_with_length(
            factory,
            function_prototype,
            "setMonth",
            date::date_set_month,
            2.0,
        );
        let set_utc_month = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCMonth",
            date::date_set_utc_month,
            2.0,
        );
        let set_full_year = builtin_function_with_length(
            factory,
            function_prototype,
            "setFullYear",
            date::date_set_full_year,
            3.0,
        );
        let set_utc_full_year = builtin_function_with_length(
            factory,
            function_prototype,
            "setUTCFullYear",
            date::date_set_utc_full_year,
            3.0,
        );
        let to_iso_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toISOString",
            date::date_to_iso_string,
        );
        let to_json = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toJSON",
            date::date_to_json,
        );
        let to_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toString",
            date::date_to_string,
        );
        let to_utc_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toUTCString",
            date::date_to_utc_string,
        );
        let to_gmt_string = to_utc_string;
        let to_date_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toDateString",
            date::date_to_date_string,
        );
        let to_time_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toTimeString",
            date::date_to_time_string,
        );
        let to_locale_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toLocaleString",
            date::date_to_locale_string,
        );
        let to_locale_date_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toLocaleDateString",
            date::date_to_locale_string,
        );
        let to_locale_time_string = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            "toLocaleTimeString",
            date::date_to_locale_string,
        );
        let to_primitive = builtin_function_with_length(
            factory,
            function_prototype,
            "[Symbol.toPrimitive]",
            date::date_to_primitive,
            1.0,
        );

        let date_prototype = {
            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Date(DateObjectInfo::default()),
                prototype: object_prototype,
                property: make_property_map!(
                    getHours => true, false, true : get_hours,
                    getMinutes => true, false, true : get_minutes,
                    getTime => true, false, true : get_time,
                    valueOf => true, false, true : value_of,
                    getFullYear => true, false, true : get_full_year,
                    getUTCFullYear => true, false, true : get_utc_full_year,
                    getMonth => true, false, true : get_month,
                    getUTCMonth => true, false, true : get_utc_month,
                    getDate => true, false, true : get_date,
                    getUTCDate => true, false, true : get_utc_date,
                    getDay => true, false, true : get_day,
                    getUTCDay => true, false, true : get_utc_day,
                    getUTCHours => true, false, true : get_utc_hours,
                    getUTCMinutes => true, false, true : get_utc_minutes,
                    getSeconds => true, false, true : get_seconds,
                    getUTCSeconds => true, false, true : get_utc_seconds,
                    getMilliseconds => true, false, true : get_milliseconds,
                    getUTCMilliseconds => true, false, true : get_utc_milliseconds,
                    getTimezoneOffset => true, false, true : get_timezone_offset,
                    getYear => true, false, true : get_year,
                    setTime => true, false, true : set_time,
                    setYear => true, false, true : set_year,
                    setMilliseconds => true, false, true : set_milliseconds,
                    setUTCMilliseconds => true, false, true : set_utc_milliseconds,
                    setSeconds => true, false, true : set_seconds,
                    setUTCSeconds => true, false, true : set_utc_seconds,
                    setMinutes => true, false, true : set_minutes,
                    setUTCMinutes => true, false, true : set_utc_minutes,
                    setHours => true, false, true : set_hours,
                    setUTCHours => true, false, true : set_utc_hours,
                    setDate => true, false, true : set_date,
                    setUTCDate => true, false, true : set_utc_date,
                    setMonth => true, false, true : set_month,
                    setUTCMonth => true, false, true : set_utc_month,
                    setFullYear => true, false, true : set_full_year,
                    setUTCFullYear => true, false, true : set_utc_full_year,
                    toISOString => true, false, true : to_iso_string,
                    toJSON => true, false, true : to_json,
                    toString => true, false, true : to_string,
                    toUTCString => true, false, true : to_utc_string,
                    toGMTString => true, false, true : to_gmt_string,
                    toDateString => true, false, true : to_date_string,
                    toTimeString => true, false, true : to_time_string,
                    toLocaleString => true, false, true : to_locale_string,
                    toLocaleDateString => true, false, true : to_locale_date_string,
                    toLocaleTimeString => true, false, true : to_locale_time_string
                ),
                property_order: Vec::new(),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_TO_PRIMITIVE_ID,
                        Property::new_data(DataProperty::new(to_primitive).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let symbol_prototype = {
            let description = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "get description",
                builtins::symbol::symbol_prototype_description,
            );
            let to_string = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "toString",
                builtins::symbol::symbol_prototype_to_string,
            );
            let value_of = Value::builtin_function_with_proto(
                &mut factory.memory_allocator,
                function_prototype,
                "valueOf",
                builtins::symbol::symbol_prototype_value_of,
            );
            let to_primitive = builtin_function_with_length(
                factory,
                function_prototype,
                "[Symbol.toPrimitive]",
                builtins::symbol::symbol_prototype_value_of,
                1.0,
            );
            let to_string_tag = factory.string("Symbol");
            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Ordinary,
                prototype: object_prototype,
                // TODO: https://tc39.github.io/ecma262/#sec-properties-of-the-symbol-prototype-object
                property: {
                    let mut property = make_property_map!();
                    property.insert(
                        "toString".to_string(),
                        Property::new_data(
                            DataProperty::new(to_string)
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    property.insert(
                        "valueOf".to_string(),
                        Property::new_data(
                            DataProperty::new(value_of)
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    property.insert(
                        "description".to_string(),
                        Property::Accessor(AccessorProperty {
                            get: description,
                            set: Value::undefined(),
                            enumerable: false,
                            configurable: true,
                        }),
                    );
                    property
                },
                property_order: vec![
                    "toString".to_string(),
                    "valueOf".to_string(),
                    "description".to_string(),
                ],
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: {
                    let mut property = FxHashMap::default();
                    property.insert(
                        SYMBOL_TO_PRIMITIVE_ID,
                        Property::new_data(DataProperty::new(to_primitive).set_configurable()),
                    );
                    property.insert(
                        SYMBOL_TO_STRING_TAG_ID,
                        Property::new_data(DataProperty::new(to_string_tag).set_configurable()),
                    );
                    property
                },
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };

        let error_prototype = {
            let name = factory.string("Error");
            let message = factory.string("");
            Value::Object(factory.alloc(Object {
                kind: ObjectKind::Error(ErrorObjectInfo {
                    stack_trace: "".to_string(),
                }),
                prototype: object_prototype,
                // TODO: https://tc39.github.io/ecma262/#sec-properties-of-the-error-prototype-object
                property: make_property_map!(
                    name => true, false, true: name,
                    message => true, false, true: message
                ),
                property_order: make_property_order!(
                    name => true, false, true: name,
                    message => true, false, true: message
                ),
                private_elements: rustc_hash::FxHashMap::default(),
                sym_property: FxHashMap::default(),
                sym_property_order: Vec::new(),
                extensible: true,
            }))
        };
        let aggregate_error_prototype =
            native_error_prototype(factory, error_prototype, "AggregateError");
        let eval_error_prototype = native_error_prototype(factory, error_prototype, "EvalError");
        let range_error_prototype = native_error_prototype(factory, error_prototype, "RangeError");
        let reference_error_prototype =
            native_error_prototype(factory, error_prototype, "ReferenceError");
        let syntax_error_prototype =
            native_error_prototype(factory, error_prototype, "SyntaxError");
        let type_error_prototype = native_error_prototype(factory, error_prototype, "TypeError");
        let uri_error_prototype = native_error_prototype(factory, error_prototype, "URIError");

        ObjectPrototypes {
            object: object_prototype,
            function: function_prototype,
            string: string_prototype,
            iterator: iterator_prototype,
            generator: generator_prototype,
            async_generator: async_generator_prototype,
            string_iterator: string_iterator_prototype,
            number: object_prototype,
            bigint: object_prototype,
            boolean: object_prototype,
            array: array_prototype,
            regexp: object_prototype,
            array_buffer: object_prototype,
            shared_array_buffer: object_prototype,
            data_view: object_prototype,
            array_iterator: array_iterator_prototype,
            map: map_prototype,
            map_iterator: map_iterator_prototype,
            set: set_prototype,
            set_iterator: set_iterator_prototype,
            weak_map: weak_map_prototype,
            weak_set: weak_set_prototype,
            date: date_prototype,
            symbol: symbol_prototype,
            error: error_prototype,
            aggregate_error: aggregate_error_prototype,
            eval_error: eval_error_prototype,
            range_error: range_error_prototype,
            reference_error: reference_error_prototype,
            syntax_error: syntax_error_prototype,
            type_error: type_error_prototype,
            uri_error: uri_error_prototype,
        }
    }

    pub fn dummy() -> Self {
        ObjectPrototypes {
            object: Value::undefined(),
            function: Value::undefined(),
            string: Value::undefined(),
            iterator: Value::undefined(),
            generator: Value::undefined(),
            async_generator: Value::undefined(),
            string_iterator: Value::undefined(),
            number: Value::undefined(),
            bigint: Value::undefined(),
            boolean: Value::undefined(),
            array: Value::undefined(),
            regexp: Value::undefined(),
            array_buffer: Value::undefined(),
            shared_array_buffer: Value::undefined(),
            data_view: Value::undefined(),
            array_iterator: Value::undefined(),
            map: Value::undefined(),
            map_iterator: Value::undefined(),
            set: Value::undefined(),
            set_iterator: Value::undefined(),
            weak_map: Value::undefined(),
            weak_set: Value::undefined(),
            date: Value::undefined(),
            symbol: Value::undefined(),
            error: Value::undefined(),
            aggregate_error: Value::undefined(),
            eval_error: Value::undefined(),
            range_error: Value::undefined(),
            reference_error: Value::undefined(),
            syntax_error: Value::undefined(),
            type_error: Value::undefined(),
            uri_error: Value::undefined(),
        }
    }
}

fn native_error_prototype(factory: &mut Factory, error_prototype: Value, name: &str) -> Value {
    let name = factory.string(name);
    let message = factory.string("");
    Value::Object(factory.alloc(Object {
        kind: ObjectKind::Error(ErrorObjectInfo {
            stack_trace: "".to_string(),
        }),
        prototype: error_prototype,
        property: make_property_map!(
            name => true, false, true: name,
            message => true, false, true: message
        ),
        property_order: make_property_order!(
            name => true, false, true: name,
            message => true, false, true: message
        ),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }))
}
