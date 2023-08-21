use ecow::EcoString;

use self::{comment::Comment, ident::Ident, op::AssignOp};

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
    Dot,
    Ellipsis,
    Semicolon,
    Colon,
    Comma,
    Plus,
    Minus,
    Asterisk,
    Div,
    Mod,
    Exp,
    LShift,
    RShift,
    URShift,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    NullishCoalescing,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    AssignOp(AssignOp),
    PlusPlus,
    MinusMinus,
    Exclamation,
    Question,
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
