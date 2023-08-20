use ecow::EcoString;

use self::ident::Ident;

pub mod ident;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(Ident),
    Whitespace(EcoString),
    LineTerminator(EcoString),
}
