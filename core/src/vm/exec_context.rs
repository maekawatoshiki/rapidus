#![macro_use]
use crate::builtins;
use crate::vm::{
    error::{ErrorKind, RuntimeError},
    jsvalue::function::{FuncInfoRef, UserFunctionInfo},
    jsvalue::symbol::SYMBOL_TO_STRING_TAG_ID,
    jsvalue::value::{BoxedValue, DataProperty, ObjectKind, Property, Value},
    vm::{CallMode, Factory, VMResult},
};
use rustc_hash::FxHashMap;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, Copy)]
pub struct LexicalEnvironmentRef(pub *mut LexicalEnvironment);

#[derive(Debug, Clone)]
pub struct ExecContext {
    //pub func_id: FunctionId, // 0 => global scope, n => function id
    //pub module_func_id: FunctionId,
    pub pc: usize,
    pub current_inst_pc: usize,
    pub run_until_pc: Option<usize>,
    pub stack: Vec<BoxedValue>,
    pub object_rest_exclusion_stack: Vec<Value>,
    pub pending_reference: Option<PendingReference>,
    pub pending_iterator_close_stack: Vec<Value>,
    pub func_ref: FuncInfoRef,
    pub callee: Value,
    pub new_target: Value,
    //pub bytecode: ByteCode,
    //pub exception_table: Vec<Exception>,
    /// This value in the context.
    pub this: Value,

    /// If true, calling JS function as a constructor.
    pub constructor_call: bool,

    pub call_mode: CallMode,
    /// If true, calling JS function as a module.
    //    pub module_call: bool,

    /// If true, calling JS function from native function.
    //    pub escape: bool,
    pub variable_environment: LexicalEnvironmentRef,
    pub lexical_environment: LexicalEnvironmentRef,
    pub saved_lexical_environment: Vec<LexicalEnvironmentRef>,
}

#[derive(Debug, Clone)]
pub struct LexicalEnvironment {
    pub record: EnvironmentRecord,
    pub outer: Option<LexicalEnvironmentRef>,
    pub immutable_names: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum EnvironmentRecord {
    Declarative(FxHashMap<String, Value>),
    Object(Value),
    Global(Value),
    Module {
        this: Value,
        record: FxHashMap<String, Value>,
        // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-module-environment-records
    },
    Function {
        this: Value,
        record: FxHashMap<String, Value>,
        // TODO: https://www.ecma-international.org/ecma-262/6.0/#sec-function-environment-records
    },
}

impl ExecContext {
    pub fn new(
        var_env: LexicalEnvironmentRef,
        lex_env: LexicalEnvironmentRef,
        func_ref: FuncInfoRef,
        this: Value,
        call_mode: CallMode,
        callee: Value,
        new_target: Value,
    ) -> Self {
        ExecContext {
            pc: 0,
            current_inst_pc: 0,
            run_until_pc: None,
            stack: vec![],
            object_rest_exclusion_stack: vec![],
            pending_reference: None,
            pending_iterator_close_stack: vec![],
            func_ref,
            callee,
            new_target,
            this,
            constructor_call: false,
            call_mode,
            variable_environment: var_env,
            lexical_environment: lex_env,
            saved_lexical_environment: vec![],
        }
    }

    pub fn empty() -> Self {
        ExecContext {
            pc: 0,
            current_inst_pc: 0,
            run_until_pc: None,
            stack: vec![],
            object_rest_exclusion_stack: vec![],
            pending_reference: None,
            pending_iterator_close_stack: vec![],
            func_ref: FuncInfoRef::default(),
            callee: Value::undefined(),
            new_target: Value::undefined(),
            this: Value::undefined(),
            constructor_call: false,
            call_mode: CallMode::Ordinary,
            variable_environment: LexicalEnvironmentRef::new_null(),
            lexical_environment: LexicalEnvironmentRef::new_null(),
            saved_lexical_environment: vec![],
        }
    }

    pub fn lex_env(&self) -> &LexicalEnvironment {
        &*self.lexical_environment
    }

    pub fn lex_env_mut(&mut self) -> &mut LexicalEnvironment {
        &mut *self.lexical_environment
    }

    pub fn constructor_call(mut self, is_constructor: bool) -> Self {
        self.constructor_call = is_constructor;
        self
    }

    fn append_function(&mut self, factory: &mut Factory, info: FuncInfoRef) {
        let name = info.func_name.clone().unwrap();
        let val = factory.function(info, self.lexical_environment);
        self.lex_env_mut().set_own_value(name, val).unwrap();
        self.initial_trace(&mut factory.memory_allocator.roots);
    }

    fn append_variable_to_var_env(&mut self, name: String) {
        let var_env = &mut self.variable_environment;
        var_env.set_own_value(name, Value::undefined()).unwrap(); // TODO: unwrap()
    }

    fn append_variable_to_lex_env(&mut self, name: String) {
        let lex_env = &mut self.lexical_environment;
        lex_env.set_own_value(name, Value::uninitialized()).unwrap(); // TODO: unwrap()
    }

    pub fn append_from_function_info(&mut self, factory: &mut Factory, info: &UserFunctionInfo) {
        for f in &info.func_decls {
            self.append_function(factory, *f);
        }

        for name in &info.var_names {
            self.append_variable_to_var_env(name.clone())
        }

        for name in &info.lex_names {
            self.append_variable_to_lex_env(name.clone())
        }
    }

    pub fn error_general(&self, msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::General(msg.into()), self)
    }

    pub fn error_type(&self, msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Type(msg.into()), self)
    }

    pub fn error_range(&self, msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Range(msg.into()), self)
    }

    pub fn error_syntax(&self, msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Syntax(msg.into()), self)
    }

    pub fn error_uri(&self, msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Uri(msg.into()), self)
    }

    pub fn error_reference(&self, msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Reference(msg.into()), self)
    }

    pub fn error_exception(&self, val: Value) -> RuntimeError {
        RuntimeError::new(ErrorKind::Exception(val), self)
    }

    pub fn error_unknown(&self) -> RuntimeError {
        RuntimeError::new(ErrorKind::Unknown, self)
    }
}

#[derive(Debug, Clone)]
pub enum PendingReference {
    Binding {
        name: String,
        env: LexicalEnvironmentRef,
    },
    Object {
        key: Value,
        object: Value,
    },
    Unresolvable {
        name: String,
    },
}

impl LexicalEnvironment {
    pub fn new_declarative(outer: Option<LexicalEnvironmentRef>) -> Self {
        LexicalEnvironment {
            record: EnvironmentRecord::Declarative(FxHashMap::default()),
            outer,
            immutable_names: Vec::new(),
        }
    }

    pub fn new_object(object: Value, outer: Option<LexicalEnvironmentRef>) -> Self {
        LexicalEnvironment {
            record: EnvironmentRecord::Object(object),
            outer,
            immutable_names: Vec::new(),
        }
    }

    pub fn new_global_initialized(factory: &mut Factory) -> Self {
        let log = factory.builtin_function("log", builtins::console::console_log);
        let parse_float = factory.builtin_function("parseFloat", builtins::parse_float);
        let parse_int = factory.builtin_function("parseInt", builtins::parse_int);
        let is_nan = factory.builtin_function("isNaN", builtins::is_nan);
        let is_finite = factory.builtin_function("isFinite", builtins::is_finite);
        let escape = factory.builtin_function("escape", builtins::escape);
        let unescape = factory.builtin_function("unescape", builtins::unescape);
        let decode_uri = factory.builtin_function("decodeURI", builtins::decode_uri);
        let decode_uri_component =
            factory.builtin_function("decodeURIComponent", builtins::decode_uri_component);
        let encode_uri = factory.builtin_function("encodeURI", builtins::encode_uri);
        let encode_uri_component =
            factory.builtin_function("encodeURIComponent", builtins::encode_uri_component);
        let eval = factory.builtin_function("eval", builtins::eval);
        let dynamic_import = factory.builtin_function("import", builtins::dynamic_import);
        let require = factory.builtin_function("require", builtins::require);
        let deep_seq = factory.builtin_function("__assert_deep_seq", builtins::deep_seq);
        let test262_detach =
            factory.builtin_function("detachArrayBuffer", builtins::test262_detach_array_buffer);
        let test262_gc = factory.builtin_function("gc", builtins::test262_gc);
        let test262_create_realm =
            factory.builtin_function("createRealm", builtins::test262_create_realm);
        let test262_eval_script =
            factory.builtin_function("evalScript", builtins::test262_eval_script);
        let test262_drain_promise_jobs =
            factory.builtin_function("__drain_promise_jobs", builtins::test262_drain_promise_jobs);
        escape.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(1.0))),
        );
        unescape.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(1.0))),
        );
        for func in [
            decode_uri,
            decode_uri_component,
            encode_uri,
            encode_uri_component,
        ] {
            func.get_object_info().insert_property(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
        }
        let console = make_normal_object!(factory,
            log => true, false, true: log
        );
        let test262 = make_normal_object!(factory,
            detachArrayBuffer => true, false, true: test262_detach,
            gc => true, false, true: test262_gc,
            createRealm => true, false, true: test262_create_realm,
            evalScript => true, false, true: test262_eval_script
        );
        let object_constructor = builtins::object::object(factory);
        let function_constructor = builtins::function::function(factory);
        let boolean_constructor = builtins::boolean::boolean(factory);
        let number_constructor = builtins::number::number(factory);
        let bigint_constructor = builtins::bigint::bigint(factory);
        for (name, func) in [("parseFloat", parse_float), ("parseInt", parse_int)] {
            number_constructor.get_object_info().insert_property(
                name.to_string(),
                Property::new_data(DataProperty::new(func).set_writable().set_configurable()),
            );
        }
        let string_constructor = builtins::string::string(factory);
        for (name, func) in [
            (
                "fromCharCode",
                builtins::string::string_from_char_code as builtins::BuiltinFuncTy,
            ),
            ("fromCodePoint", builtins::string::string_from_code_point),
            ("raw", builtins::string::string_raw),
        ] {
            let builtin = factory.builtin_function(name, func);
            builtin.get_object_info().insert_property(
                "length".to_string(),
                Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
            );
            string_constructor.get_object_info().insert_property(
                name.to_string(),
                Property::new_data(DataProperty::new(builtin).set_writable().set_configurable()),
            );
        }
        let array_constructor = builtins::array::array(factory);
        let iterator_constructor = builtins::iterator::iterator(factory);
        let array_buffer_constructor = builtins::array_buffer::array_buffer(factory);
        let shared_array_buffer_constructor = builtins::array_buffer::shared_array_buffer(factory);
        let data_view_constructor = builtins::data_view::data_view(factory);
        let typed_array_constructors = builtins::typed_array::typed_array_constructors(factory);
        let map_constructor = builtins::collection::map(factory);
        let set_constructor = builtins::collection::set(factory);
        let weak_map_constructor = builtins::collection::weak_map(factory);
        let weak_set_constructor = builtins::collection::weak_set(factory);
        let weak_ref_constructor = builtins::weak_ref::weak_ref(factory);
        let finalization_registry_constructor =
            builtins::finalization_registry::finalization_registry(factory);
        let shadow_realm_constructor = builtins::shadow_realm::shadow_realm(factory);
        let date_constructor = builtins::date::date(factory);
        let promise_constructor = builtins::promise::promise(factory);
        let symbol_constructor = builtins::symbol::symbol(factory);
        let temporal_object = builtins::temporal::temporal(factory);
        let error_constructor = builtins::error::error(factory);
        let aggregate_error_constructor = builtins::error::aggregate_error(factory);
        let eval_error_constructor = builtins::error::eval_error(factory);
        let type_error_constructor = builtins::error::type_error(factory);
        let range_error_constructor = builtins::error::range_error(factory);
        let reference_error_constructor = builtins::error::reference_error(factory);
        let syntax_error_constructor = builtins::error::syntax_error(factory);
        let uri_error_constructor = builtins::error::uri_error(factory);
        for constructor in [
            aggregate_error_constructor,
            eval_error_constructor,
            type_error_constructor,
            range_error_constructor,
            reference_error_constructor,
            syntax_error_constructor,
            uri_error_constructor,
        ] {
            constructor.get_object_info().prototype = error_constructor;
        }
        let regexp_constructor = builtins::regexp(factory);
        let json_object = factory.object(rustc_hash::FxHashMap::default());
        let json_parse = factory.builtin_function("parse", builtins::json::json_parse);
        let json_stringify = factory.builtin_function("stringify", builtins::json::json_stringify);
        json_parse.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
        );
        json_stringify.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(3.0)).set_configurable()),
        );
        json_object.get_object_info().insert_property(
            "parse".to_string(),
            Property::new_data(
                DataProperty::new(json_parse)
                    .set_writable()
                    .set_configurable(),
            ),
        );
        json_object.get_object_info().insert_property(
            "stringify".to_string(),
            Property::new_data(
                DataProperty::new(json_stringify)
                    .set_writable()
                    .set_configurable(),
            ),
        );
        let json_tag = factory.string("JSON");
        crate::builtins::helpers::define_well_known_symbol_property(
            factory,
            json_object,
            SYMBOL_TO_STRING_TAG_ID,
            Property::new_data(DataProperty::new(json_tag).set_configurable()),
        );
        let math_object = builtins::math::math(factory);
        let reflect_object = builtins::reflect::reflect(factory);
        let intl_object = builtins::intl::intl(factory);
        let atomics_object = builtins::atomics::atomics(factory);
        let proxy_constructor = builtins::proxy::proxy(factory);
        let global = make_normal_object!(
            factory,
            undefined  => false,false,false: Value::undefined(),
            NaN        => false,false,false: Value::Number(::std::f64::NAN),
            Infinity   => false,false,false: Value::Number(::std::f64::INFINITY),
            require    => true, false, true: require,
            __assert_deep_seq    => true, false, true: deep_seq,
            __drain_promise_jobs => true, false, true: test262_drain_promise_jobs,
            parseFloat => true, false, true: parse_float,
            parseInt   => true, false, true: parse_int,
            isNaN      => true, false, true: is_nan,
            isFinite   => true, false, true: is_finite,
            escape     => true, false, true: escape,
            unescape   => true, false, true: unescape,
            decodeURI  => true, false, true: decode_uri,
            decodeURIComponent => true, false, true: decode_uri_component,
            encodeURI  => true, false, true: encode_uri,
            encodeURIComponent => true, false, true: encode_uri_component,
            eval       => true, false, true: eval,
            console    => true, false, true: console,
            Object     => true, false, true: object_constructor,
            Function   => true, false, true: function_constructor,
            Boolean    => true, false, true: boolean_constructor,
            Number     => true, false, true: number_constructor,
            BigInt     => true, false, true: bigint_constructor,
            String     => true, false, true: string_constructor,
            Array      => true, false, true: array_constructor,
            Iterator   => true, false, true: iterator_constructor,
            ArrayBuffer => true, false, true: array_buffer_constructor,
            SharedArrayBuffer => true, false, true: shared_array_buffer_constructor,
            DataView   => true, false, true: data_view_constructor,
            Map        => true, false, true: map_constructor,
            Set        => true, false, true: set_constructor,
            WeakMap    => true, false, true: weak_map_constructor,
            WeakSet    => true, false, true: weak_set_constructor,
            WeakRef    => true, false, true: weak_ref_constructor,
            FinalizationRegistry => true, false, true: finalization_registry_constructor,
            ShadowRealm => true, false, true: shadow_realm_constructor,
            Date       => true, false, true: date_constructor,
            Promise    => true, false, true: promise_constructor,
            Proxy      => true, false, true: proxy_constructor,
            Symbol     => true, false, true: symbol_constructor,
            Temporal   => true, false, true: temporal_object,
            RegExp     => true, false, true: regexp_constructor,
            JSON       => true, false, true: json_object,
            Reflect    => true, false, true: reflect_object,
            Intl       => true, false, true: intl_object,
            Atomics    => true, false, true: atomics_object,
            Error      => true, false, true: error_constructor,
            AggregateError => true, false, true: aggregate_error_constructor,
            EvalError  => true, false, true: eval_error_constructor,
            TypeError  => true, false, true: type_error_constructor,
            RangeError => true, false, true: range_error_constructor,
            ReferenceError  => true, false, true: reference_error_constructor,
            SyntaxError  => true, false, true: syntax_error_constructor,
            URIError  => true, false, true: uri_error_constructor,
            Math       => true, false, true: math_object
        );
        global.get_object_info().insert_property(
            "import".to_string(),
            Property::new_data(
                DataProperty::new(dynamic_import)
                    .set_writable()
                    .set_configurable(),
            ),
        );
        for (name, constructor) in typed_array_constructors {
            global.get_object_info().insert_property(
                name.to_string(),
                Property::new_data(
                    DataProperty::new(constructor)
                        .set_writable()
                        .set_configurable(),
                ),
            );
        }
        global.get_object_info().insert_property(
            "$262".to_string(),
            Property::new_data(DataProperty::new(test262).set_writable().set_configurable()),
        );
        global.get_object_info().insert_property(
            "globalThis".to_string(),
            Property::new_data(DataProperty::new(global).set_writable().set_configurable()),
        );
        LexicalEnvironment {
            record: EnvironmentRecord::Global(global),
            outer: None,
            immutable_names: Vec::new(),
        }
    }

    pub fn get_value(&self, name: &str) -> Result<Value, RuntimeError> {
        match self.record {
            EnvironmentRecord::Function { ref record, .. }
            | EnvironmentRecord::Module { ref record, .. }
            | EnvironmentRecord::Declarative(ref record) => match record.get(name) {
                Some(binding) if binding == &Value::uninitialized() => {
                    return Err(RuntimeError::reference(format!(
                        "'{}' is not defined",
                        name
                    )));
                }
                Some(binding) => return Ok(*binding),
                None => {}
            },
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                if obj.has_own_property(name) {
                    let val = obj.get_property(name);
                    if val == Value::uninitialized() {
                        return Err(RuntimeError::reference(format!(
                            "'{}' is not defined",
                            name
                        )));
                    }
                    return Ok(val);
                }
            }
        };

        if let Some(outer) = self.outer {
            outer.get_value(name)
        } else {
            Err(RuntimeError::reference(format!(
                "'{}' is not defined",
                name
            )))
        }
    }

    pub fn set_value(&mut self, name: &str, val: Value) -> VMResult {
        if self.immutable_names.iter().any(|saved| saved == name)
            && !self.binding_is_uninitialized(name)
        {
            return Err(RuntimeError::typeerr(format!(
                "Assignment to constant variable '{}'",
                name
            )));
        }
        match self.record {
            EnvironmentRecord::Function { ref mut record, .. } => match record.get_mut(name) {
                Some(binding) => {
                    *binding = val;
                    if let Some(arguments) = record.get("arguments").copied() {
                        if arguments.is_object() {
                            let argument_index = {
                                let obj = arguments.get_object_info();
                                match obj.kind {
                                    ObjectKind::Arguments(ref info) => info
                                        .parameter_map
                                        .iter()
                                        .find(|(_, param_name)| param_name.as_str() == name)
                                        .map(|(index, _)| index.clone()),
                                    _ => None,
                                }
                            };
                            if let Some(index) = argument_index {
                                arguments.set_property(index, val);
                            }
                        }
                    }
                    return Ok(());
                }
                None => {}
            },
            EnvironmentRecord::Module { ref mut record, .. }
            | EnvironmentRecord::Declarative(ref mut record) => match record.get_mut(name) {
                Some(binding) => {
                    *binding = val;
                    return Ok(());
                }
                None => {}
            },
            EnvironmentRecord::Global(obj) => {
                obj.set_property(name, val);
                return Ok(());
            }
            EnvironmentRecord::Object(obj) => {
                if obj.has_own_property(name) {
                    obj.set_property(name, val);
                    return Ok(());
                }
            }
        };

        if let Some(mut outer) = self.outer {
            outer.set_value(name, val)
        } else {
            Err(RuntimeError::reference(format!(
                "Assignment to undeclared identifier '{}'",
                name
            )))
        }
    }

    pub fn binding_is_uninitialized(&self, name: &str) -> bool {
        match self.record {
            EnvironmentRecord::Function { ref record, .. }
            | EnvironmentRecord::Module { ref record, .. }
            | EnvironmentRecord::Declarative(ref record) => {
                record.get(name) == Some(&Value::uninitialized())
            }
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                obj.get_property(name) == Value::uninitialized()
            }
        }
    }

    pub fn set_own_value(&mut self, name: impl Into<String>, val: Value) -> VMResult {
        match self.record {
            EnvironmentRecord::Function { ref mut record, .. }
            | EnvironmentRecord::Module { ref mut record, .. }
            | EnvironmentRecord::Declarative(ref mut record) => {
                record.insert(name.into(), val);
            }
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                obj.set_property(name, val);
            }
        };
        return Ok(());
    }

    pub fn get_global_object(&self) -> Value {
        match self.record {
            EnvironmentRecord::Global(obj) => obj,
            _ => panic!(),
        }
    }

    pub fn get_this_binding(&self) -> Value {
        match self.record {
            EnvironmentRecord::Function { this, .. } => this,
            EnvironmentRecord::Global(obj) => obj,
            _ => {
                if let Some(outer) = self.outer {
                    outer.get_this_binding()
                } else {
                    Value::undefined()
                }
            }
        }
    }

    pub fn set_this_binding(&mut self, val: Value) -> VMResult {
        match self.record {
            EnvironmentRecord::Function { ref mut this, .. }
            | EnvironmentRecord::Module { ref mut this, .. } => {
                *this = val;
                Ok(())
            }
            _ => {
                if let Some(mut outer) = self.outer {
                    outer.set_this_binding(val)
                } else {
                    Err(RuntimeError::reference("this"))
                }
            }
        }
    }
}

impl LexicalEnvironmentRef {
    pub fn new_null() -> Self {
        LexicalEnvironmentRef(::std::ptr::null_mut())
    }

    pub fn as_ptr(self) -> *mut LexicalEnvironment {
        self.0
    }
}

impl Deref for LexicalEnvironmentRef {
    type Target = LexicalEnvironment;

    fn deref(&self) -> &LexicalEnvironment {
        unsafe { &*self.as_ptr() }
    }
}

impl DerefMut for LexicalEnvironmentRef {
    fn deref_mut(&mut self) -> &mut LexicalEnvironment {
        unsafe { &mut *self.as_ptr() }
    }
}
