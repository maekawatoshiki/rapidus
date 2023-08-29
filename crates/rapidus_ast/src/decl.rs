use crate::{span::Span, Node};

/// Represents a declaration.
/// https://tc39.es/ecma262/multipage/ecmascript-language-statements-and-declarations.html#prod-Declaration
#[derive(Debug, Clone)]
pub struct Decl {
    span: Span,
    kind: Kind,
}

#[derive(Debug, Clone)]
pub enum Kind {
    LexicalDecl(LexicalDecl),
}

/// https://tc39.es/ecma262/multipage/ecmascript-language-statements-and-declarations.html#prod-LexicalDeclaration
#[derive(Debug, Clone)]
pub struct LexicalDecl {
    span: Span,
}

impl Decl {
    pub fn new(span: Span, kind: Kind) -> Self {
        Self { span, kind }
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl LexicalDecl {
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Node for LexicalDecl {
    fn span(&self) -> Span {
        self.span
    }
}

impl Node for Decl {
    fn span(&self) -> Span {
        self.span
    }
}
