use ecow::EcoString;

/// Represents a string literal.
#[derive(Debug, Clone, PartialEq)]
pub struct Template {
    /// Escaped string value
    val: EcoString,

    /// Original string literal
    raw: EcoString,
}

impl Template {
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
