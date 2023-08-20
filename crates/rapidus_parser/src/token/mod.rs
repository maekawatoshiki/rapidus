use self::ident::Ident;

mod ident;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(Ident),
}
