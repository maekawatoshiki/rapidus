use rapidus_ast::span::{Span, Spanned};
use thiserror::Error as ThisError;

use crate::token::Token;

#[derive(Debug, ThisError)]
pub enum Error {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error("Syntax error: {0}")]
    SyntaxError(SyntaxError),

    #[error("TODO")]
    Todo(Span),
}

#[derive(Debug, ThisError)]
pub enum SyntaxError {
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,

    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Spanned<Token>),

    #[error("Unknown error: {0}")]
    Unknown(&'static str),
}
