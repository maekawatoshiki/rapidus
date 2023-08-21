use ecow::EcoString;

use self::{
    comment::Comment,
    ident::Ident,
    op::{AssignOp, Op},
};

pub mod comment;
pub mod ident;
pub mod op;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Op(Op),
    AssignOp(AssignOp),
    Arrow,
    Ident(Ident),
    Whitespace(EcoString),
    LineTerminator(EcoString),
    Comment(Comment),
}

pub fn is_line_terminator(c: char) -> bool {
    c == '\n' || c == '\r' || c == '\u{2028}' || c == '\u{2029}'
}

pub fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\x0c' || c == '\u{00A0}' || c == '\u{FEFF}'
}
