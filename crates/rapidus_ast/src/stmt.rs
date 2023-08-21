use crate::{expr::Expr, span::Span, Node};

/// Represents a statement.
/// https://tc39.es/ecma262/multipage/ecmascript-language-statements-and-declarations.html#prod-Statement
#[derive(Debug, Clone)]
pub struct Stmt {
    span: Span,
    kind: Kind,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Empty,
    Expr(Expr),
}

impl Stmt {
    pub const fn new(span: Span, kind: Kind) -> Self {
        Self { span, kind }
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl Node for Stmt {
    fn span(&self) -> Span {
        self.span
    }
}
