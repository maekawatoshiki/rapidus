use rustc_hash::FxHashMap;
use vm::value::Value2;

#[derive(Debug, Clone)]
pub struct Frame {
    pub execution_context: ExecutionContext,
}

#[derive(Debug, Clone)]
pub struct ExecutionContext {
    pub variable_environment: *mut LexicalEnvironment,
    pub lexical_environment: *mut LexicalEnvironment,
}

#[derive(Debug, Clone)]
pub struct LexicalEnvironment {
    pub record: EnvironmentRecord,
    pub outer: Option<*mut LexicalEnvironment>,
}

#[derive(Debug, Clone)]
pub enum EnvironmentRecord {
    Declarative(FxHashMap<String, Value2>),
    Object(Value2),
}

// impl Frame {
//     pub fn new() -> Self {
//         Frame {}
//     }
// }

impl ExecutionContext {
    pub fn new(env: *mut LexicalEnvironment) -> Self {
        ExecutionContext {
            variable_environment: env,
            lexical_environment: env,
        }
    }
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
}
