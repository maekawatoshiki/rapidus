use crate::{literal::Literal, span::Span, Node};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
}

impl Node for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span(),
        }
    }
}
