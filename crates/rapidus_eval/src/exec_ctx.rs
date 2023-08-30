use crate::{
    env::{Environment, ModuleEnv},
    value::JsValue,
};

/// https://tc39.es/ecma262/multipage/executable-code-and-execution-contexts.html#sec-execution-contexts
#[derive(Debug, Clone)]
pub struct ExecutionCtx {
    env: Environment,

    #[allow(dead_code)]
    function: JsValue,
}

impl ExecutionCtx {
    pub fn new() -> Self {
        Self {
            env: Environment::Module(ModuleEnv::new()), // TODO
            function: JsValue::null(),
        }
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn env_mut(&mut self) -> &mut Environment {
        &mut self.env
    }
}
