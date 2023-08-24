use crate::{span::Span, Node};

use ecow::EcoString;

/// Represents a literal.
/// https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#prod-Literal
#[derive(Debug, Clone)]
pub enum Literal {
    Num(Num),
    Null(Null),
}

/// Represents a numeric literal.
#[derive(Debug, Clone)]
pub struct Num {
    span: Span,
    val: f64,
    raw: EcoString,
}

/// Represents a null literal.
#[derive(Debug, Clone)]
pub struct Null {
    span: Span,
}

impl Node for Literal {
    fn span(&self) -> Span {
        match self {
            Self::Num(num) => num.span(),
            Self::Null(null) => null.span(),
        }
    }
}

impl Node for Num {
    fn span(&self) -> Span {
        self.span
    }
}

impl Node for Null {
    fn span(&self) -> Span {
        self.span
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

impl Null {
    pub const fn new(span: Span) -> Self {
        Self { span }
    }
}

impl From<Num> for Literal {
    fn from(num: Num) -> Self {
        Self::Num(num)
    }
}

impl From<Null> for Literal {
    fn from(null: Null) -> Self {
        Self::Null(null)
    }
}
