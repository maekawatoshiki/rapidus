#![macro_use]

use bytecode_gen::ByteCode;
use gc;
use rustc_hash::FxHashMap;
use vm::codegen::FunctionInfo;
use vm::error::RuntimeError;
use vm::jsvalue::function::Exception;
use vm::jsvalue::object::{DataProperty, ObjectInfo, ObjectKind2, Property2};
use vm::jsvalue::prototype::ObjectPrototypes;
use vm::jsvalue::value::Value2;
use vm::vm::VMResult;

pub type LexicalEnvironmentRef = *mut LexicalEnvironment;

#[derive(Debug, Clone)]
pub struct Frame {
    pub execution_context: ExecutionContext,
    pub pc: usize,
    pub saved_stack_len: usize,
    pub bytecode: ByteCode,
    pub exception_table: Vec<Exception>,
    pub this: Value2,
    pub constructor_call: bool,
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
    Declarative(FxHashMap<String, Value2>),
    Object(Value2),
    Global(Value2),
    // TODO: Function...
}

impl Frame {
    pub fn new(
        execution_context: ExecutionContext,
        bytecode: ByteCode,
        exception_table: Vec<Exception>,
        this: Value2,
        constructor_call: bool,
    ) -> Self {
        Frame {
            execution_context,
            pc: 0,
            saved_stack_len: 0,
            bytecode,
            exception_table,
            this,
            constructor_call,
            escape: false,
        }
    }

    pub fn new_empty_with_this(this: Value2, constructor_call: bool) -> Self {
        Frame {
            execution_context: ExecutionContext::new_empty(),
            pc: 0,
            saved_stack_len: 0,
            bytecode: vec![],
            exception_table: vec![],
            this,
            constructor_call,
            escape: false,
        }
    }

    pub fn lex_env(&self) -> &LexicalEnvironment {
        unsafe { &*self.execution_context.lexical_environment }
    }

    pub fn lex_env_mut(&mut self) -> &mut LexicalEnvironment {
        unsafe { &mut *self.execution_context.lexical_environment }
    }

    pub fn escape(mut self) -> Self {
        self.escape = true;
        self
    }

    pub fn saved_stack_len(mut self, saved_stack_len: usize) -> Self {
        self.saved_stack_len = saved_stack_len;
        self
    }

    pub fn append_function(&mut self, memory_allocator: &mut gc::MemoryAllocator, f: Value2) {
        let mut val = f.copy_object(memory_allocator);
        let name = val.as_function().name.clone().unwrap();
        val.set_function_outer_environment(self.execution_context.lexical_environment);
        self.lex_env_mut().set_own_value(name, val).unwrap();
        use gc::GcTarget;
        self.execution_context
            .initial_trace(&mut memory_allocator.roots);
    }

    pub fn append_variable_to_var_env(&mut self, name: String) {
        let var_env = unsafe { &mut *self.execution_context.variable_environment };
        var_env.set_own_value(name, Value2::undefined()).unwrap(); // TODO: unwrap()
    }

    pub fn append_variable_to_lex_env(&mut self, name: String) {
        let lex_env = unsafe { &mut *self.execution_context.lexical_environment };
        lex_env
            .set_own_value(name, Value2::uninitialized())
            .unwrap(); // TODO: unwrap()
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
    pub fn new(env: *mut LexicalEnvironment) -> Self {
        ExecutionContext {
            variable_environment: env,
            lexical_environment: env,
            saved_lexical_environment: vec![],
        }
    }

    pub fn new_empty() -> Self {
        ExecutionContext {
            variable_environment: ::std::ptr::null_mut(),
            lexical_environment: ::std::ptr::null_mut(),
            saved_lexical_environment: vec![],
        }
    }
}

#[macro_export]
macro_rules! make_global_env {
    ($($property_name:ident : $val:expr),*) => { {
        let mut record = FxHashMap::default();
        $( record.insert((stringify!($property_name)).to_string(), $val); )*
        record
    } };
}

impl LexicalEnvironment {
    pub fn new_declarative(outer: Option<*mut LexicalEnvironment>) -> Self {
        LexicalEnvironment {
            record: EnvironmentRecord::Declarative(FxHashMap::default()),
            outer,
        }
    }

    pub fn new_object(object: Value2, outer: Option<*mut LexicalEnvironment>) -> Self {
        LexicalEnvironment {
            record: EnvironmentRecord::Object(object),
            outer,
        }
    }

    pub fn new_global_initialized(
        memory_allocator: &mut gc::MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
    ) -> Self {
        use builtin::parse_float;
        use builtins;

        let log = Value2::builtin_function(
            memory_allocator,
            object_prototypes,
            "log".to_string(),
            builtins::console::builtin_log,
        );
        let parse_float = Value2::builtin_function(
            memory_allocator,
            object_prototypes,
            "parseFloat".to_string(),
            parse_float,
        );
        let console = make_normal_object!(memory_allocator,
            log => true, false, true: log
        );
        let object_constructor = builtins::object::object(memory_allocator, object_prototypes);
        let function_constructor =
            builtins::function::function(memory_allocator, object_prototypes);
        let array_constructor = builtins::array::array(memory_allocator, object_prototypes);
        LexicalEnvironment {
            record: EnvironmentRecord::Global(make_normal_object!(
                memory_allocator,
                undefined  => false,false,false: Value2::undefined(),
                NaN        => false,false,false: Value2::Number(::std::f64::NAN),
                Infinity   => false,false,false: Value2::Number(::std::f64::INFINITY),
                parseFloat => true, false, true: parse_float,
                console    => true, false, true: console,
                Object     => true, false, true: object_constructor,
                Function   => true, false, true: function_constructor,
                Array      => true, false, true: array_constructor
            )),
            outer: None,
        }
    }

    pub fn get_value(&self, name: &String) -> Result<Value2, RuntimeError> {
        match self.record {
            EnvironmentRecord::Declarative(ref record) => match record.get(name) {
                Some(binding) if binding == &Value2::uninitialized() => {
                    return Err(RuntimeError::Reference(format!(
                        "'{}' is not defined",
                        name
                    )));
                }
                Some(binding) => return Ok(*binding),
                None => {}
            },
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj)
                if obj.has_own_property(name.as_str()) =>
            {
                let val = obj.get_property_by_str_key(name.as_str());
                if val == Value2::uninitialized() {
                    return Err(RuntimeError::Reference(format!(
                        "'{}' is not defined",
                        name
                    )));
                }
                return Ok(val);
            }
            _ => {}
        };

        if let Some(outer) = self.get_outer() {
            outer.get_value(name)
        } else {
            Err(RuntimeError::Reference(format!(
                "'{}' is not defined",
                name
            )))
        }
    }

    pub fn set_value(&mut self, name: String, val: Value2) -> VMResult {
        match self.record {
            EnvironmentRecord::Declarative(ref mut record) => match record.get_mut(&name) {
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

        if let Some(outer) = self.get_outer() {
            outer.set_value(name, val)
        } else {
            Err(RuntimeError::Reference(format!(
                "Assignment to undeclared identifier '{}'",
                name
            )))
        }
    }

    pub fn set_own_value(&mut self, name: String, val: Value2) -> VMResult {
        match self.record {
            EnvironmentRecord::Declarative(ref mut record) => {
                record.insert(name, val);
            }
            EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                obj.set_property_by_string_key(name, val);
            }
        };
        return Ok(());
    }

    pub fn get_outer(&self) -> Option<&mut LexicalEnvironment> {
        self.outer.and_then(|outer| Some(unsafe { &mut *outer }))
    }

    pub fn get_global_object(&self) -> Value2 {
        match self.record {
            EnvironmentRecord::Global(obj) => obj,
            _ => panic!(),
        }
    }
}
