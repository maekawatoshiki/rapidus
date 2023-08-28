use ecow::EcoString;

use self::{
    comment::Comment,
    ident::Ident,
    num::Num,
    op::{AssignOp, Op},
    str::Str,
    template::Template,
};

pub mod comment;
pub mod ident;
pub mod num;
pub mod op;
pub mod str;
pub mod template;

#[derive(Debug, Clone, PartialEq)]
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
    Num(Num),
    Str(Str),
    Template(Template),
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

#[macro_export]
macro_rules! t {
    (";") => {
        Token::Op(Op::Semicolon)
    };
    ("+") => {
        Token::Op(Op::Plus)
    };
    ("-") => {
        Token::Op(Op::Minus)
    };
    ("(") => {
        Token::LParen
    };
    (")") => {
        Token::RParen
    };
}
