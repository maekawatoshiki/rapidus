use thiserror::Error as ThisError;

#[derive(Debug, ThisError)]
pub enum Error {
    #[error("TODO")]
    Todo,

    #[error("ReferenceError")]
    ReferenceError,
}
