use ecow::EcoString;

/// Represents a string literal.
#[derive(Debug, Clone, PartialEq)]
pub struct Str {
    /// Escaped string value
    pub(crate) val: EcoString,

    /// Original string literal
    pub(crate) raw: EcoString,
}

impl Str {
    pub const fn new(val: EcoString, raw: EcoString) -> Self {
        Self { val, raw }
    }

    pub fn val(&self) -> &EcoString {
        &self.val
    }

    pub fn raw(&self) -> &EcoString {
        &self.raw
    }
}
