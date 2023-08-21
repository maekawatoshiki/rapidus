use crate::{span::Span, Node};

use ecow::EcoString;

/// Represents a literal.
/// https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#prod-Literal
#[derive(Debug, Clone)]
pub enum Literal {
    Num(Num),
}

/// Represents a numeric literal.
#[derive(Debug, Clone)]
pub struct Num {
    span: Span,
    val: f64,
    raw: EcoString,
}

impl Node for Num {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Num {
    pub const fn new(span: Span, val: f64, raw: EcoString) -> Self {
        Self { span, val, raw }
    }

    pub const fn val(&self) -> f64 {
        self.val
    }

    pub const fn raw(&self) -> &EcoString {
        &self.raw
    }
}
