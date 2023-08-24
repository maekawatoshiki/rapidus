use rapidus_ast::span::Span;
use thiserror::Error as ThisError;

#[derive(Debug, ThisError)]
pub enum Error {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error("Syntax error")]
    SyntaxError,

    #[error("TODO")]
    Todo(Span),
}
