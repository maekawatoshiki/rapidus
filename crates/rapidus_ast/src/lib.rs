use span::Span;

pub mod decl;
pub mod expr;
pub mod bin;
pub mod ident;
pub mod literal;
pub mod module;
pub mod span;
pub mod stmt;

pub trait Node {
    fn span(&self) -> Span;
}
