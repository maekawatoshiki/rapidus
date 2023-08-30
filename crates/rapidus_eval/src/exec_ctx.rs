use crate::value::JsValue;

/// https://tc39.es/ecma262/multipage/executable-code-and-execution-contexts.html#sec-execution-contexts
#[derive(Debug, Clone)]
pub struct ExecutionCtx {
    env_idx: usize,

    #[allow(dead_code)]
    function: JsValue,
}

impl ExecutionCtx {
    pub fn new(env_idx: usize) -> Self {
        Self {
            env_idx,
            function: JsValue::null(),
        }
    }

    pub fn env_idx(&self) -> usize {
        self.env_idx
    }
}
