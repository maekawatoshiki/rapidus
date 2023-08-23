use rapidus_ast::module::Module;

use crate::error::Error;

pub struct EvalCtx {}

impl EvalCtx {
    pub const fn new() -> Self {
        Self {}
    }

    pub fn eval_module(&mut self, _module: &Module) -> Result<(), Error> {
        Err(Error::Todo)
    }
}
