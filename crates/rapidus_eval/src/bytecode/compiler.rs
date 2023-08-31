use rapidus_ast::{
    bin::BinOpExpr,
    decl::{self, Decl},
    expr::Expr,
    ident::Ident,
    literal::Literal,
    module::{Module, ModuleItem},
    stmt::{self, Stmt},
};
use thiserror::Error as ThisError;

use crate::eval::EvalCtx;

pub struct ModuleCompiler {}

struct StmtCompiler {}

struct DeclCompiler {}

struct ExprCompiler {}

#[derive(Debug, Clone, ThisError)]
pub enum Error {}

impl ModuleCompiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, module: &Module) -> Result<EvalCtx, Error> {
        for item in module.children() {
            self.compile_module_item(item)?;
        }
        todo!()
    }

    fn compile_module_item(&self, item: &ModuleItem) -> Result<(), Error> {
        match item {
            ModuleItem::Stmt(stmt) => StmtCompiler::new().compile(stmt),
            ModuleItem::Decl(decl) => DeclCompiler::new().compile(decl),
        }
    }
}

impl StmtCompiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, stmt: &Stmt) -> Result<(), Error> {
        match stmt.kind() {
            stmt::Kind::Expr(expr) => ExprCompiler::new().compile(expr),
            stmt::Kind::Empty => Ok(()),
        }
    }
}

impl DeclCompiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, decl: &Decl) -> Result<(), Error> {
        match decl.kind() {
            decl::Kind::LexicalDecl(_decl) => {
                todo!()
            }
        }
    }
}

impl ExprCompiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::Ident(ident) => self.compile_ident(ident),
            Expr::Literal(lit) => self.compile_lit(lit),
            Expr::BinOp(op) => self.compile_binop(op),
        }
    }

    fn compile_ident(&self, _ident: &Ident) -> Result<(), Error> {
        todo!()
    }

    fn compile_lit(&self, _lit: &Literal) -> Result<(), Error> {
        todo!()
    }

    fn compile_binop(&self, _expr: &BinOpExpr) -> Result<(), Error> {
        todo!()
    }
}
