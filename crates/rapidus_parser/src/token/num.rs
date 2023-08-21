use ecow::EcoString;

/// Represents a numeric literal.
#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub val: f64,
    pub raw: EcoString,
}

impl Num {
    pub const fn new(val: f64, raw: EcoString) -> Self {
        Self { val, raw }
    }
}
