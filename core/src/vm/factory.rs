use crate::builtins::BuiltinFuncTy;
use crate::gc;
use crate::vm::{
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::{
        function::ThisMode,
        object::{property_order_from_map, AccessorProperty, ArgumentsObjectInfo, DataProperty},
        value::{
            ArrayObjectInfo, BigIntInfo, CollectionIteratorKind, DateObjectInfo, ErrorObjectInfo,
            FuncInfoRef, FunctionObjectInfo, FunctionObjectKind, MapIteratorInfo, MapObjectInfo,
            Object, ObjectKind, Property, SetIteratorInfo, SetObjectInfo, SymbolInfo,
            UserFunctionInfo, Value, WeakMapObjectInfo, WeakSetObjectInfo,
        },
    },
    vm::{EnvironmentRecord, FunctionParameter, LexicalEnvironment, LexicalEnvironmentRef},
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
    /// Canonical instances of the well-known symbols (Symbol.iterator, ...),
    /// keyed by their SYMBOL_*_ID. Symbols compare by object identity, so
    /// every site must share these instead of allocating its own copy with
    /// symbol_with_id().
    well_known_symbols: FxHashMap<usize, Value>,
}

impl Factory {
    pub fn new(memory_allocator: gc::MemoryAllocator, object_prototypes: ObjectPrototypes) -> Self {
        let mut factory = Factory {
            memory_allocator,
            object_prototypes,
            func_refs: vec![None; 30],
            next_func_id: 1,
            well_known_symbols: FxHashMap::default(),
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
        Value::String(self.alloc(body.into()))
    }

    /// Generate Value for an object.
    pub fn object(&mut self, property: FxHashMap<String, Property>) -> Value {
        let property_order = property_order_from_map(&property);
        self.object_with_property_order(property, property_order)
    }

    pub fn object_with_property_order(
        &mut self,
        property: FxHashMap<String, Property>,
        property_order: Vec<String>,
    ) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Ordinary,
            prototype: self.object_prototypes.object,
            property,
            property_order,
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    /// Generate Value for an `arguments` object.
    pub fn arguments(
        &mut self,
        property: FxHashMap<String, Property>,
        parameter_map: FxHashMap<String, String>,
    ) -> Value {
        let property_order = property_order_from_map(&property);
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Arguments(ArgumentsObjectInfo { parameter_map }),
            prototype: self.object_prototypes.object,
            property,
            property_order,
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    /// Generate Value for a JS function.
    pub fn function(
        &mut self,
        info: FuncInfoRef,
        outer_env: impl Into<Option<LexicalEnvironmentRef>>,
    ) -> Value {
        let name_prop = self.string(info.func_name.clone().unwrap_or("".to_string()));
        let prototype = self.object(FxHashMap::default());

        let f = Value::Object(self.alloc(Object {
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length    => false, false, true : Value::Number(info.length as f64),
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            property_order: make_property_order!(
                length    => false, false, true : Value::Number(info.length as f64),
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            kind: ObjectKind::Function(FunctionObjectInfo {
                name: info.func_name.clone(),
                super_constructor: None,
                kind: FunctionObjectKind::User {
                    info,
                    outer_env: outer_env.into(),
                },
            }),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }));

        f.get_property("prototype")
            .get_object_info()
            .insert_property("constructor".to_string(), Property::new_data_simple(f));

        f
    }

    /// Generate Value for a built-in (native) function.
    pub fn builtin_function(&mut self, name: impl Into<String>, func: BuiltinFuncTy) -> Value {
        let name: String = name.into();
        let name_prop = self.string(name.clone());
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Function(FunctionObjectInfo {
                name: Some(name),
                super_constructor: None,
                kind: FunctionObjectKind::Builtin(func),
            }),
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            property_order: make_property_order!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn array(&mut self, elems: Vec<Property>) -> Value {
        let length = elems.len();
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Array(ArrayObjectInfo {
                elems,
                length,
                length_writable: true,
            }),
            prototype: self.object_prototypes.array,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn map(&mut self) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Map(MapObjectInfo { entries: vec![] }),
            prototype: self.object_prototypes.map,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn set(&mut self) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Set(SetObjectInfo { entries: vec![] }),
            prototype: self.object_prototypes.set,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn weak_map(&mut self) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::WeakMap(WeakMapObjectInfo { entries: vec![] }),
            prototype: self.object_prototypes.weak_map,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn weak_set(&mut self) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::WeakSet(WeakSetObjectInfo { entries: vec![] }),
            prototype: self.object_prototypes.weak_set,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn map_iterator(&mut self, map: Value, kind: CollectionIteratorKind) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::MapIterator(MapIteratorInfo {
                iterated_map: map,
                next_index: 0,
                kind,
            }),
            prototype: self.object_prototypes.map_iterator,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn set_iterator(&mut self, set: Value, kind: CollectionIteratorKind) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::SetIterator(SetIteratorInfo {
                iterated_set: set,
                next_index: 0,
                kind,
            }),
            prototype: self.object_prototypes.set_iterator,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn date(&mut self) -> Value {
        self.date_from_info(DateObjectInfo::default())
    }

    pub fn date_from_millis(&mut self, millis: f64) -> Value {
        self.date_from_info(DateObjectInfo::from_millis(millis))
    }

    fn date_from_info(&mut self, info: DateObjectInfo) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Date(info),
            prototype: self.object_prototypes.date,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn symbol(&mut self, description: Option<String>) -> Value {
        self.symbol_with_id(crate::id::get_unique_id(), description)
    }

    /// Returns the canonical Value for a well-known symbol id. The instance
    /// is created once, locked as a GC root, and shared by the global Symbol
    /// constructor and every builtin that defines a symbol-keyed property,
    /// so that e.g. Object.getOwnPropertySymbols(x)[0] === Symbol.toStringTag
    /// holds.
    pub fn well_known_symbol(&mut self, id: usize) -> Value {
        use crate::vm::jsvalue::symbol::*;
        if let Some(sym) = self.well_known_symbols.get(&id) {
            return *sym;
        }
        let description = match id {
            SYMBOL_ASYNC_ITERATOR_ID => "Symbol.asyncIterator",
            SYMBOL_HAS_INSTANCE_ID => "Symbol.hasInstance",
            SYMBOL_IS_CONCAT_SPREADABLE_ID => "Symbol.isConcatSpreadable",
            SYMBOL_ITERATOR_ID => "Symbol.iterator",
            SYMBOL_MATCH_ID => "Symbol.match",
            SYMBOL_MATCH_ALL_ID => "Symbol.matchAll",
            SYMBOL_REPLACE_ID => "Symbol.replace",
            SYMBOL_SEARCH_ID => "Symbol.search",
            SYMBOL_SPECIES_ID => "Symbol.species",
            SYMBOL_SPLIT_ID => "Symbol.split",
            SYMBOL_TO_PRIMITIVE_ID => "Symbol.toPrimitive",
            SYMBOL_TO_STRING_TAG_ID => "Symbol.toStringTag",
            SYMBOL_UNSCOPABLES_ID => "Symbol.unscopables",
            _ => panic!("well_known_symbol: not a well-known symbol id"),
        };
        let sym = self.symbol_with_id(id, Some(description.to_string()));
        self.memory_allocator.lock_value(sym);
        self.well_known_symbols.insert(id, sym);
        sym
    }

    pub fn symbol_with_id(&mut self, id: usize, description: Option<String>) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Symbol(SymbolInfo {
                id,
                description,
                registered: false,
            }),
            prototype: self.object_prototypes.symbol,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn bigint(&mut self, decimal: impl Into<String>) -> Value {
        Value::Object(self.alloc(Object {
            kind: ObjectKind::BigInt(BigIntInfo {
                decimal: decimal.into(),
            }),
            prototype: self.object_prototypes.bigint,
            property: make_property_map!(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn error(&mut self, message: impl Into<String>) -> Value {
        self.native_error("Error", message)
    }

    pub fn native_error(&mut self, name: &str, message: impl Into<String>) -> Value {
        let message = self.string(message.into());
        let prototype = self.native_error_prototype(name);
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Error(ErrorObjectInfo::new()),
            prototype,
            property: make_property_map!(
                message => true, false, true: message
            ),
            property_order: make_property_order!(
                message => true, false, true: message
            ),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    pub fn native_error_without_message(&mut self, name: &str) -> Value {
        let prototype = self.native_error_prototype(name);
        Value::Object(self.alloc(Object {
            kind: ObjectKind::Error(ErrorObjectInfo::new()),
            prototype,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    }

    fn native_error_prototype(&self, name: &str) -> Value {
        let prototype = match name {
            "AggregateError" => self.object_prototypes.aggregate_error,
            "EvalError" => self.object_prototypes.eval_error,
            "RangeError" => self.object_prototypes.range_error,
            "ReferenceError" => self.object_prototypes.reference_error,
            "SyntaxError" => self.object_prototypes.syntax_error,
            "TypeError" => self.object_prototypes.type_error,
            "URIError" => self.object_prototypes.uri_error,
            _ => self.object_prototypes.error,
        };
        prototype
    }

    pub fn generate_builtin_constructor(
        &mut self,
        constructor_name: impl Into<String>,
        constructor_func: BuiltinFuncTy,
        prototype: Value,
    ) -> Value {
        let ary = self.builtin_function(constructor_name, constructor_func);
        ary.get_object_info().property.insert(
            "prototype".to_string(),
            Property::new_data(DataProperty::new(prototype)),
        );
        ary.get_property("prototype").set_constructor(ary);
        ary
    }
}

impl Factory {
    pub fn create_declarative_environment<F>(
        &mut self,
        f: F,
        outer: Option<LexicalEnvironmentRef>,
    ) -> LexicalEnvironmentRef
    where
        F: Fn(&mut Factory, &mut FxHashMap<String, Value>),
    {
        let env = LexicalEnvironment {
            record: EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                f(self, &mut record);
                record
            }),
            outer,
            immutable_names: Vec::new(),
        };

        LexicalEnvironmentRef(self.alloc(env))
    }

    pub fn create_variable_environment(
        &mut self,
        var_names: &Vec<String>,
        outer_env_ref: LexicalEnvironmentRef,
    ) -> LexicalEnvironmentRef {
        self.create_declarative_environment(
            |_, record| {
                for name in var_names {
                    record.insert(name.clone(), Value::undefined());
                }
            },
            Some(outer_env_ref),
        )
    }

    pub fn create_lexical_environment(
        &mut self,
        lex_names: &Vec<String>,
        immutable_names: &Vec<String>,
        outer_env_ref: LexicalEnvironmentRef,
    ) -> LexicalEnvironmentRef {
        let mut env = self.create_declarative_environment(
            |_, record| {
                for name in lex_names {
                    record.insert(name.clone(), Value::uninitialized());
                }
            },
            Some(outer_env_ref),
        );
        env.immutable_names = immutable_names.clone();
        env
    }

    pub fn create_function_environment(
        &mut self,
        callee: Value,
        user_func: FuncInfoRef,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
    ) -> LexicalEnvironmentRef {
        let mut record = FxHashMap::default();

        let not_arrow_func = user_func.this_mode != ThisMode::Lexical;
        if not_arrow_func {
            let strict = user_func.this_mode == ThisMode::Strict;
            let simple_parameters = !user_func
                .params
                .iter()
                .any(|param| param.rest_param || param.has_initializer);
            let parameter_map = if strict || !simple_parameters {
                FxHashMap::default()
            } else {
                user_func
                    .params
                    .iter()
                    .enumerate()
                    .filter(|(_, param)| !param.rest_param)
                    .map(|(i, param)| (i.to_string(), param.name.clone()))
                    .collect::<FxHashMap<String, String>>()
            };
            let thrower = if strict {
                Some(self.builtin_function("ThrowTypeError", crate::builtins::throw_type_error))
            } else {
                None
            };
            let arguments = self.arguments(
                {
                    let mut props: FxHashMap<String, Property> = args
                        .iter()
                        .enumerate()
                        .map(|(i, &arg)| {
                            (
                                Value::Number(i as f64).to_string(),
                                DataProperty::new(arg)
                                    .set_writable()
                                    .set_enumerate()
                                    .set_configurable()
                                    .into(),
                            )
                        })
                        .collect();
                    props.insert(
                        "length".to_string(),
                        Property::new_data(
                            DataProperty::new(Value::Number(args.len() as f64))
                                .set_writable()
                                .set_configurable(),
                        ),
                    );
                    if strict {
                        let thrower = thrower.unwrap();
                        props.insert(
                            "callee".to_string(),
                            Property::Accessor(AccessorProperty {
                                get: thrower,
                                set: thrower,
                                enumerable: false,
                                configurable: false,
                            }),
                        );
                    } else {
                        props.insert(
                            "callee".to_string(),
                            Property::new_data(
                                DataProperty::new(callee).set_writable().set_configurable(),
                            ),
                        );
                    }
                    props
                },
                parameter_map,
            );
            record.insert("arguments".to_string(), arguments);
        }

        for name in &user_func.var_names {
            record.insert(name.clone(), Value::undefined());
        }

        for (
            i,
            FunctionParameter {
                name, rest_param, ..
            },
        ) in user_func.params.iter().enumerate()
        {
            record.insert(
                name.clone(),
                if *rest_param {
                    self.array(
                        (*args)
                            .get(i..)
                            .unwrap_or(&vec![])
                            .iter()
                            .map(|elem| Property::new_data_simple(*elem))
                            .collect::<Vec<Property>>(),
                    )
                } else {
                    *args.get(i).unwrap_or(&Value::undefined())
                },
            );
        }

        let env = LexicalEnvironment {
            record: EnvironmentRecord::Function { record, this },
            outer: outer_env,
            immutable_names: Vec::new(),
        };

        LexicalEnvironmentRef(self.alloc(env))
    }
}
