use crate::literal::Literal;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
}
