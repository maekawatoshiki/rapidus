use rapidus_ast::module::Module;

use crate::{error::Error, value::JsValue};

// TODO: What is the relationship between this and Realm?
pub struct EvalCtx {}

impl EvalCtx {
    pub const fn new() -> Self {
        Self {}
    }

    pub fn eval_module(&mut self, _module: &Module) -> Result<JsValue, Error> {
        Ok(JsValue::undefined())
    }
}
