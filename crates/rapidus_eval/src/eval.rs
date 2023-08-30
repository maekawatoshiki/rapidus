use rapidus_ast::{
    bin::{BinOp, BinOpExpr},
    decl::{self, Decl},
    expr::Expr,
    ident::Ident,
    literal::Literal,
    module::{Module, ModuleItem},
    stmt::{self, Stmt},
};

use crate::{
    env::EnvRecord, error::Error, exec_ctx::ExecutionCtx, object::string::JsString, value::JsValue,
};

// TODO: What is the relationship between this and Realm?
pub struct EvalCtx {
    #[allow(dead_code)]
    exec_ctx_stack: Vec<ExecutionCtx>,
}

impl EvalCtx {
    pub fn new() -> Self {
        Self {
            exec_ctx_stack: vec![ExecutionCtx::new()],
        }
    }

    pub fn eval_module(&mut self, module: &Module) -> Result<JsValue, Error> {
        let mut result = JsValue::undefined();
        for item in module.children() {
            result = self.eval_module_item(item)?;
        }
        Ok(result)
    }

    fn eval_module_item(&mut self, item: &ModuleItem) -> Result<JsValue, Error> {
        match item {
            ModuleItem::Stmt(stmt) => self.eval_stmt(stmt),
            ModuleItem::Decl(decl) => self.eval_decl(decl),
        }
    }

    fn eval_decl(&mut self, decl: &Decl) -> Result<JsValue, Error> {
        match decl.kind() {
            decl::Kind::LexicalDecl(decl) => {
                let name = decl.bind();
                self.running_exec_ctx_mut()
                    .env_mut()
                    .create_mutable_binding(name.clone());
                if let Some(init) = decl.init() {
                    let init = self.eval_expr(init)?;
                    self.running_exec_ctx_mut()
                        .env_mut()
                        .initialize_binding(name.clone(), init);
                }
            }
        }
        Ok(JsValue::undefined())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<JsValue, Error> {
        match stmt.kind() {
            stmt::Kind::Expr(expr) => self.eval_expr(expr),
            stmt::Kind::Empty => Ok(JsValue::undefined()),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<JsValue, Error> {
        match expr {
            Expr::Ident(ident) => self.eval_ident(ident),
            Expr::Literal(lit) => self.eval_literal(lit),
            Expr::BinOp(op) => self.eval_binop(op),
        }
    }

    fn eval_binop(&mut self, expr: &BinOpExpr) -> Result<JsValue, Error> {
        // TODO: https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-applystringornumericbinaryoperator
        let lhs = self.eval_expr(expr.lhs())?;
        let rhs = self.eval_expr(expr.rhs())?;
        assert!(lhs.is_f64());
        assert!(rhs.is_f64());
        match expr.op() {
            BinOp::Add => Ok(JsValue::f64(lhs.as_f64().unwrap() + rhs.as_f64().unwrap())),
            BinOp::Sub => Ok(JsValue::f64(lhs.as_f64().unwrap() - rhs.as_f64().unwrap())),
            BinOp::Mul => Ok(JsValue::f64(lhs.as_f64().unwrap() * rhs.as_f64().unwrap())),
            BinOp::Div => Ok(JsValue::f64(lhs.as_f64().unwrap() / rhs.as_f64().unwrap())),
            BinOp::Mod => Ok(JsValue::f64(lhs.as_f64().unwrap() % rhs.as_f64().unwrap())),
        }
    }

    fn eval_ident(&mut self, ident: &Ident) -> Result<JsValue, Error> {
        match ident.val().as_str() {
            "undefined" => Ok(JsValue::undefined()),
            name => self
                .running_exec_ctx()
                .env()
                .get_binding_value(name)
                .ok_or_else(|| Error::ReferenceError),
        }
    }

    fn eval_literal(&mut self, lit: &Literal) -> Result<JsValue, Error> {
        match lit {
            // TODO: https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-identifiers-runtime-semantics-evaluation
            Literal::Num(num) => Ok(JsValue::f64(num.val())),
            Literal::Str(str) => Ok(JsValue::str(
                JsString::new_leaked(str.val()) as *mut _ as *const _
            )),
            Literal::Null(_null) => Ok(JsValue::null()),
        }
    }

    fn running_exec_ctx(&self) -> &ExecutionCtx {
        self.exec_ctx_stack.last().unwrap()
    }

    fn running_exec_ctx_mut(&mut self) -> &mut ExecutionCtx {
        self.exec_ctx_stack.last_mut().unwrap()
    }
}
