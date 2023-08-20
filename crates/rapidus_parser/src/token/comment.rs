use ecow::EcoString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comment {
    SingleLine(EcoString),
    MultiLine(EcoString),
}
