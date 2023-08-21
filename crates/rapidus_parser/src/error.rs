use thiserror::Error as ThisError;

#[derive(Debug, ThisError)]
pub enum Error {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error("TODO")]
    Todo,
}
