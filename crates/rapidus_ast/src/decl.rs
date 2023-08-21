use crate::{span::Span, Node};

/// Represents a declaration.
/// https://tc39.es/ecma262/multipage/ecmascript-language-statements-and-declarations.html#prod-Declaration
#[derive(Debug, Clone)]
pub struct Decl {
    span: Span,
}

impl Node for Decl {
    fn span(&self) -> Span {
        self.span
    }
}
