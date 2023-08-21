use ecow::EcoString;

#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    val: f64,
    raw: EcoString,
}

impl Num {
    pub const fn new(val: f64, raw: EcoString) -> Self {
        Self { val, raw }
    }

    pub fn val(&self) -> f64 {
        self.val
    }

    pub fn raw(&self) -> &EcoString {
        &self.raw
    }
}
