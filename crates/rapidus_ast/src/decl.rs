use ecow::EcoString;

use crate::{expr::Expr, span::Span, Node};

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
    bind: Box<EcoString>,
    init: Option<Box<Expr>>,
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
    pub fn new(span: Span, bind: EcoString, init: Option<Expr>) -> Self {
        Self {
            span,
            bind: Box::new(bind),
            init: init.map(Box::new),
        }
    }

    pub fn bind(&self) -> &EcoString {
        &self.bind
    }

    pub fn init(&self) -> Option<&Expr> {
        self.init.as_deref()
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
