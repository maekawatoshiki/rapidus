use ecow::EcoString;

#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub val: f64,
    pub raw: EcoString,
}
