use ecow::EcoString;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Keyword(Keyword),
    Ident(EcoString),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {}
