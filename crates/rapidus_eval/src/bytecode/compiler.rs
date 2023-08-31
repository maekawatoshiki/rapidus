use std::borrow::Cow;

use rapidus_ast::{
    bin::{BinOp, BinOpExpr},
    decl::{self, Decl},
    expr::Expr,
    ident::Ident,
    literal::Literal,
    module::{Module, ModuleItem},
    stmt::{self, Stmt},
};
use thiserror::Error as ThisError;

use crate::{
    env::{Environment, ModuleEnv},
    eval::EvalCtx,
    exec_ctx::ExecutionCtx,
    object::string::JsString,
    value::JsValue,
};

use super::{code::Code, insn};

pub struct ModuleCompiler {
    code: Code,
}

struct StmtCompiler<'a> {
    code: &'a mut Code,
}

struct DeclCompiler<'a> {
    #[allow(dead_code)]
    code: &'a mut Code,
}

struct ExprCompiler<'a> {
    code: &'a mut Code,
}

#[derive(Debug, Clone, ThisError)]
pub enum Error {
    #[error("Todo: {0}")]
    Todo(Cow<'static, str>),
}

impl ModuleCompiler {
    pub fn new() -> Self {
        Self { code: Code::new() }
    }

    pub fn compile(mut self, module: &Module) -> Result<EvalCtx, Error> {
        for (item, drop_value) in module
            .children()
            .iter()
            .enumerate()
            // To retain the last value
            .map(|(i, item)| (item, i != module.children().len() - 1))
        {
            self.compile_module_item(item, drop_value)?;
        }

        Ok(EvalCtx::new(
            ExecutionCtx::new(self.code, 0),
            Environment::Module(ModuleEnv::new()),
            vec![],
        ))
    }

    fn compile_module_item(&mut self, item: &ModuleItem, drop_value: bool) -> Result<(), Error> {
        match item {
            ModuleItem::Stmt(stmt) => StmtCompiler::new(&mut self.code).compile(stmt, drop_value),
            ModuleItem::Decl(decl) => DeclCompiler::new(&mut self.code).compile(decl),
        }
    }
}

impl<'a> StmtCompiler<'a> {
    pub fn new(code: &'a mut Code) -> Self {
        Self { code }
    }

    pub fn compile(&mut self, stmt: &Stmt, drop_value: bool) -> Result<(), Error> {
        match stmt.kind() {
            stmt::Kind::Expr(expr) => {
                ExprCompiler::new(self.code).compile(expr)?;
                if drop_value {
                    self.code.push_opcode(insn::DROP);
                }
                Ok(())
            }
            stmt::Kind::Empty => Ok(()),
        }
    }
}

impl<'a> DeclCompiler<'a> {
    pub fn new(code: &'a mut Code) -> Self {
        Self { code }
    }

    pub fn compile(&mut self, decl: &Decl) -> Result<(), Error> {
        match decl.kind() {
            decl::Kind::LexicalDecl(_decl) => Err(Error::Todo("decl".into())),
        }
    }
}

impl<'a> ExprCompiler<'a> {
    pub fn new(code: &'a mut Code) -> Self {
        Self { code }
    }

    pub fn compile(&mut self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::BinOp(op) => self.compile_binop(op),
            Expr::Literal(lit) => self.compile_lit(lit),
            Expr::Ident(ident) => self.compile_ident(ident),
        }
    }

    fn compile_binop(&mut self, expr: &BinOpExpr) -> Result<(), Error> {
        self.compile(expr.lhs())?;
        self.compile(expr.rhs())?;
        match expr.op() {
            BinOp::Add => self.code.push_opcode(insn::ADD),
            BinOp::Sub => self.code.push_opcode(insn::SUB),
            BinOp::Mul => self.code.push_opcode(insn::MUL),
            BinOp::Div => self.code.push_opcode(insn::DIV),
            BinOp::Mod => self.code.push_opcode(insn::MOD),
        }
        Ok(())
    }

    fn compile_lit(&mut self, lit: &Literal) -> Result<(), Error> {
        match lit {
            Literal::Num(num) => {
                self.code.push_opcode(insn::CONST_F64);
                self.code.push_f64(num.val());
                Ok(())
            }
            Literal::Str(str) => {
                let idx = self.code.push_lit(JsValue::str(
                    JsString::new_leaked(str.val()) as *mut _ as *const _,
                ));
                self.code.push_opcode(insn::CONST_LIT);
                self.code.push_bytes(idx.to_le_bytes());
                Ok(())
            }
            Literal::Null(_) => {
                self.code.push_opcode(insn::NULL);
                Ok(())
            }
        }
    }

    fn compile_ident(&self, _ident: &Ident) -> Result<(), Error> {
        Err(Error::Todo("expr ident".into()))
    }
}
