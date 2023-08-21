use span::Span;

pub mod decl;
pub mod module;
pub mod span;
pub mod stmt;

pub trait Node {
    fn span(&self) -> &Span;
}
