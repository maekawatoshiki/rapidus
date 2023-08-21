use crate::{span::Span, Node};

/// Represents a statement.
/// https://tc39.es/ecma262/multipage/ecmascript-language-statements-and-declarations.html#prod-Statement
#[derive(Debug, Clone)]
pub struct Stmt {
    span: Span,
}

impl Node for Stmt {
    fn span(&self) -> &Span {
        &self.span
    }
}
