use crate::{expr::Expr, span::Span, Node};

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    op: BinOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinOpExpr {
    pub fn new(span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> Self {
        Self {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }
    }

    pub fn op(&self) -> BinOp {
        self.op
    }

    pub fn lhs(&self) -> &Expr {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }
}

impl Node for BinOpExpr {
    fn span(&self) -> Span {
        self.span
    }
}
