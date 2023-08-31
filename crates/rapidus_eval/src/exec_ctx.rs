use crate::{bytecode::code::Code, value::JsValue};

/// https://tc39.es/ecma262/multipage/executable-code-and-execution-contexts.html#sec-execution-contexts
#[derive(Debug, Clone)]
pub struct ExecutionCtx {
    /// Bytecode to execute
    code: Code,

    /// Program counter for `code`
    pc: usize,

    /// Index of the base environment in the environment stack
    ///
    /// `env_stack[base_env_idx..]` is the range of environments this execution context has access
    /// to.
    base_env_idx: usize,

    #[allow(dead_code)]
    function: JsValue,
}

impl ExecutionCtx {
    pub fn new(code: Code, base_env_idx: usize) -> Self {
        Self {
            code,
            pc: 0,
            base_env_idx,
            function: JsValue::null(),
        }
    }

    pub const fn base_env_idx(&self) -> usize {
        self.base_env_idx
    }

    pub const fn code(&self) -> &Code {
        &self.code
    }

    pub const fn pc(&self) -> usize {
        self.pc
    }

    pub fn pc_mut(&mut self) -> &mut usize {
        &mut self.pc
    }
}
