use ecow::EcoString;
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
    env::{Binding, Environment, ModuleEnv},
    error::Error,
    exec_ctx::ExecutionCtx,
    object::string::JsString,
    value::JsValue,
};

// TODO: What is the relationship between this and Realm?
pub struct EvalCtx {
    #[allow(dead_code)]
    exec_ctx_stack: Vec<ExecutionCtx>,
    env_stack: Vec<Environment>,
    bindings: Vec<JsValue>,
}

impl EvalCtx {
    pub fn new() -> Self {
        Self {
            exec_ctx_stack: vec![ExecutionCtx::new(0)],
            env_stack: vec![Environment::Module(ModuleEnv::new())],
            bindings: vec![],
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
                self.create_mutable_binding(name.to_owned());
                if let Some(init) = decl.init() {
                    let init = self.eval_expr(init)?;
                    self.initialize_binding(name, init);
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
            name => self.get_binding_value(name).map_or_else(
                || Err(Error::ReferenceError),
                |b| b.ok_or(Error::ReferenceError),
            ),
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
}

impl EvalCtx {
    fn running_exec_ctx(&self) -> &ExecutionCtx {
        self.exec_ctx_stack.last().unwrap()
    }

    fn create_mutable_binding(&mut self, name: impl Into<EcoString>) {
        let env = &mut self.env_stack.last_mut().unwrap();
        let bind_idx = self.bindings.len();
        env.bindings_mut()
            .insert(name.into(), Binding::new(bind_idx, true));
        self.bindings.push(JsValue::undefined());
    }

    // TODO: Should return Result<JsValue, Error>, where Error is one of:
    //       - ReferenceError("Binding not found")
    //       - ReferenceError("Binding not initialized")
    fn get_binding_value(&self, name: impl AsRef<str>) -> Option<Option<JsValue>> {
        self.lookup_binding(name).map(|binding| {
            if binding.initialized() {
                Some(self.bindings[binding.idx()])
            } else {
                None
            }
        })
    }

    fn initialize_binding(&mut self, name: impl AsRef<str>, value: JsValue) {
        let binding = self.lookup_binding_mut(name).unwrap();
        assert!(binding.mutable());
        assert!(!binding.initialized());
        binding.set_initialized(true);
        let idx = binding.idx();
        self.bindings[idx] = value;
    }

    fn lookup_binding(&self, name: impl AsRef<str>) -> Option<Binding> {
        let exec_ctx = self.running_exec_ctx();
        let mut envs = self.env_stack[exec_ctx.base_env_idx()..].iter().rev();
        envs.find_map(|env| env.bindings().get(name.as_ref()).copied())
    }

    fn lookup_binding_mut(&mut self, name: impl AsRef<str>) -> Option<&mut Binding> {
        let base_env_idx = self.running_exec_ctx().base_env_idx();
        let mut envs = self.env_stack[base_env_idx..].iter_mut().rev();
        envs.find_map(|env| env.bindings_mut().get_mut(name.as_ref()))
    }
}
