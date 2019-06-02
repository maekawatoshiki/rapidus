#![macro_use]

use crate::bytecode_gen::ByteCode;
use crate::gc;
use crate::vm::codegen::FunctionInfo;
use crate::vm::error::RuntimeError;
use crate::vm::jsvalue::function::Exception;
use crate::vm::jsvalue::object::{DataProperty, ObjectInfo, ObjectKind, Property};
use crate::vm::jsvalue::value::Value;
use crate::vm::vm::{Factory, VMResult};
use rustc_hash::FxHashMap;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, Copy)]
pub struct LexicalEnvironmentRef(pub *mut LexicalEnvironment);

#[derive(Debug, Clone)]
pub struct Frame {
    pub id: usize, // 0 => global scope, n => function id
    pub execution_context: ExecutionContext,
    pub pc: usize,
    pub saved_stack_len: usize,
    pub bytecode: ByteCode,
    pub exception_table: Vec<Exception>,
    pub this: Value,
    /// If true, calling JS function as a constructor.
    pub constructor_call: bool,
    /// If true, calling JS function as a module.
    pub module_call: bool,
    /// If true, calling JS function from native function.
    pub escape: bool,
}

#[derive(Debug, Clone)]
pub struct ExecutionContext {
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

impl Frame {
    pub fn new(
        execution_context: ExecutionContext,
        bytecode: ByteCode,
        exception_table: Vec<Exception>,
        this: Value,
    ) -> Self {
        Frame {
            id: 0,
            execution_context,
            pc: 0,
            saved_stack_len: 0,
            bytecode,
            exception_table,
            this,
            constructor_call: false,
            module_call: false,
            escape: false,
        }
    }

    pub fn new_empty_with_this(this: Value) -> Self {
        Frame {
            id: 0,
            execution_context: ExecutionContext::new_empty(),
            pc: 0,
            saved_stack_len: 0,
            bytecode: vec![],
            exception_table: vec![],
            this,
            constructor_call: false,
            module_call: false,
            escape: false,
        }
    }

    pub fn lex_env(&self) -> &LexicalEnvironment {
        &*self.execution_context.lexical_environment
    }

    pub fn lex_env_mut(&mut self) -> &mut LexicalEnvironment {
        &mut *self.execution_context.lexical_environment
    }

    pub fn escape(mut self) -> Self {
        self.escape = true;
        self
    }

    pub fn constructor_call(mut self, is_constructor: bool) -> Self {
        self.constructor_call = is_constructor;
        self
    }

    pub fn module_call(mut self, is_module: bool) -> Self {
        self.module_call = is_module;
        self
    }

    pub fn saved_stack_len(mut self, saved_stack_len: usize) -> Self {
        self.saved_stack_len = saved_stack_len;
        self
    }

    pub fn id(mut self, id: usize) -> Self {
        self.id = id;
        self
    }

    pub fn append_function(&mut self, memory_allocator: &mut gc::MemoryAllocator, f: Value) {
        let mut val = f.copy_object(memory_allocator);
        let name = val.as_function().name.clone().unwrap();
        val.set_function_outer_environment(self.execution_context.lexical_environment);
        self.lex_env_mut().set_own_value(name, val).unwrap();
        use crate::gc::GcTarget;
        self.execution_context
            .initial_trace(&mut memory_allocator.roots);
    }

    pub fn append_variable_to_var_env(&mut self, name: String) {
        let var_env = &mut self.execution_context.variable_environment;
        var_env.set_own_value(name, Value::undefined()).unwrap(); // TODO: unwrap()
    }

    pub fn append_variable_to_lex_env(&mut self, name: String) {
        let lex_env = &mut self.execution_context.lexical_environment;
        lex_env.set_own_value(name, Value::uninitialized()).unwrap(); // TODO: unwrap()
    }

    pub fn append_from_function_info(
        &mut self,
        memory_allocator: &mut gc::MemoryAllocator,
        info: &FunctionInfo,
    ) {
        for f in &info.func_decls {
            self.append_function(memory_allocator, *f);
        }

        for name in &info.var_names {
            self.append_variable_to_var_env(name.clone())
        }

        for name in &info.lex_names {
            self.append_variable_to_lex_env(name.clone())
        }
    }
}

impl ExecutionContext {
    pub fn new(env: LexicalEnvironmentRef) -> Self {
        ExecutionContext {
            variable_environment: env,
            lexical_environment: env,
            saved_lexical_environment: vec![],
        }
    }

    pub fn new_empty() -> Self {
        ExecutionContext {
            variable_environment: LexicalEnvironmentRef::new_null(),
            lexical_environment: LexicalEnvironmentRef::new_null(),
            saved_lexical_environment: vec![],
        }
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
        use crate::builtin::{deep_seq, parse_float, require};
        use crate::builtins;

        let log = factory.builtin_function("log", builtins::console::console_log);
        let parse_float = factory.builtin_function("parseFloat", parse_float);
        let require = factory.builtin_function("require", require);
        let deep_seq = factory.builtin_function("__assert_deep_seq", deep_seq);
        let console = make_normal_object!(factory,
            log => true, false, true: log
        );
        let object_constructor = builtins::object::object(factory);
        let function_constructor = builtins::function::function(factory);
        let array_constructor = builtins::array::array(factory);
        let symbol_constructor = builtins::symbol::symbol(factory);
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
                console    => true, false, true: console,
                Object     => true, false, true: object_constructor,
                Function   => true, false, true: function_constructor,
                Array      => true, false, true: array_constructor,
                Symbol     => true, false, true: symbol_constructor,
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
                    return Err(RuntimeError::Reference(format!(
                        "'{}' is not defined",
                        name
                    )));
                }
                Some(binding) => return Ok(*binding),
                None => {}
            },
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                if obj.has_own_property(name.as_str()) {
                    let val = obj.get_property_by_str_key(name.as_str());
                    if val == Value::uninitialized() {
                        return Err(RuntimeError::Reference(format!(
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
            Err(RuntimeError::Reference(format!(
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
                obj.set_property_by_string_key(name, val);
                return Ok(());
            }
        };

        if let Some(mut outer) = self.outer {
            outer.set_value(name, val)
        } else {
            Err(RuntimeError::Reference(format!(
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
                obj.set_property_by_string_key(name, val);
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
