use ecow::EcoString;

use crate::{span::Span, Node};

#[derive(Debug, Clone)]
pub struct Ident {
    span: Span,
    val: EcoString,
}

impl Ident {
    pub const fn new(span: Span, val: EcoString) -> Self {
        Self { span, val }
    }

    pub const fn val(&self) -> &EcoString {
        &self.val
    }
}

impl Node for Ident {
    fn span(&self) -> Span {
        self.span
    }
}
