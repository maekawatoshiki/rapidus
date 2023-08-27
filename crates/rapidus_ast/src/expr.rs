use crate::{bin::BinOpExpr, ident::Ident, literal::Literal, span::Span, Node};

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    BinOp(BinOpExpr),
}

impl Node for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Ident(ident) => ident.span(),
            Self::Literal(lit) => lit.span(),
            Self::BinOp(op) => op.span(),
        }
    }
}
