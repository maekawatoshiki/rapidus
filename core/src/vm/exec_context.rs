#![macro_use]
use crate::builtins;
use crate::vm::{
    error::{ErrorKind, RuntimeError},
    jsvalue::function::{FuncInfoRef, UserFunctionInfo},
    jsvalue::value::{BoxedValue, Value},
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
    pub stack: Vec<BoxedValue>,
    pub func_ref: FuncInfoRef,
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
    ) -> Self {
        ExecContext {
            pc: 0,
            current_inst_pc: 0,
            stack: vec![],
            func_ref,
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
            stack: vec![],
            func_ref: FuncInfoRef::default(),
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

impl LexicalEnvironment {
    pub fn new_declarative(outer: Option<LexicalEnvironmentRef>) -> Self {
        LexicalEnvironment {
            record: EnvironmentRecord::Declarative(FxHashMap::default()),
            outer,
        }
    }

    pub fn new_object(object: Value, outer: Option<LexicalEnvironmentRef>) -> Self {
        LexicalEnvironment {
            record: EnvironmentRecord::Object(object),
            outer,
        }
    }

    pub fn new_global_initialized(factory: &mut Factory) -> Self {
        let log = factory.builtin_function("log", builtins::console::console_log);
        let parse_float = factory.builtin_function("parseFloat", builtins::parse_float);
        let parse_int = factory.builtin_function("parseInt", builtins::parse_int);
        let require = factory.builtin_function("require", builtins::require);
        let deep_seq = factory.builtin_function("__assert_deep_seq", builtins::deep_seq);
        let console = make_normal_object!(factory,
            log => true, false, true: log
        );
        let object_constructor = builtins::object::object(factory);
        let function_constructor = builtins::function::function(factory);
        let array_constructor = builtins::array::array(factory);
        let date_constructor = builtins::date::date(factory);
        let symbol_constructor = builtins::symbol::symbol(factory);
        let error_constructor = builtins::error::error(factory);
        let math_object = builtins::math::math(factory);
        LexicalEnvironment {
            record: EnvironmentRecord::Global(make_normal_object!(
                factory,
                undefined  => false,false,false: Value::undefined(),
                NaN        => false,false,false: Value::Number(::std::f64::NAN),
                Infinity   => false,false,false: Value::Number(::std::f64::INFINITY),
                require    => true, false, true: require,
                __assert_deep_seq    => true, false, true: deep_seq,
                parseFloat => true, false, true: parse_float,
                parseInt   => true, false, true: parse_int,
                console    => true, false, true: console,
                Object     => true, false, true: object_constructor,
                Function   => true, false, true: function_constructor,
                Array      => true, false, true: array_constructor,
                Date       => true, false, true: date_constructor,
                Symbol     => true, false, true: symbol_constructor,
                Error      => true, false, true: error_constructor,
                Math       => true, false, true: math_object
            )),
            outer: None,
        }
    }

    pub fn get_value(&self, name: impl Into<String>) -> Result<Value, RuntimeError> {
        let name = name.into();
        match self.record {
            EnvironmentRecord::Function { ref record, .. }
            | EnvironmentRecord::Module { ref record, .. }
            | EnvironmentRecord::Declarative(ref record) => match record.get(&name) {
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
                if obj.has_own_property(name.as_str()) {
                    let val = obj.get_property(name.as_str());
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

    pub fn set_value(&mut self, name: String, val: Value) -> VMResult {
        match self.record {
            EnvironmentRecord::Function { ref mut record, .. }
            | EnvironmentRecord::Module { ref mut record, .. }
            | EnvironmentRecord::Declarative(ref mut record) => match record.get_mut(&name) {
                Some(binding) => {
                    *binding = val;
                    return Ok(());
                }
                None => {}
            },
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                obj.set_property(name, val);
                return Ok(());
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
