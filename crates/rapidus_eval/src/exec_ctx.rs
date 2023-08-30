use crate::value::JsValue;

/// https://tc39.es/ecma262/multipage/executable-code-and-execution-contexts.html#sec-execution-contexts
#[derive(Debug, Clone)]
pub struct ExecutionCtx {
    base_env_idx: usize,

    #[allow(dead_code)]
    function: JsValue,
}

impl ExecutionCtx {
    pub fn new(base_env_idx: usize) -> Self {
        Self {
            base_env_idx,
            function: JsValue::null(),
        }
    }

    pub fn base_env_idx(&self) -> usize {
        self.base_env_idx
    }
}
