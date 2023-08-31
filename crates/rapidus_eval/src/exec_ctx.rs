use crate::{bytecode::code::Code, value::JsValue};

/// https://tc39.es/ecma262/multipage/executable-code-and-execution-contexts.html#sec-execution-contexts
#[derive(Debug, Clone)]
pub struct ExecutionCtx {
    code: Code,

    base_env_idx: usize,

    #[allow(dead_code)]
    function: JsValue,
}

impl ExecutionCtx {
    pub fn new(code: Code, base_env_idx: usize) -> Self {
        Self {
            code,
            base_env_idx,
            function: JsValue::null(),
        }
    }

    pub fn base_env_idx(&self) -> usize {
        self.base_env_idx
    }

    pub fn code(&self) -> &Code {
        &self.code
    }
}
