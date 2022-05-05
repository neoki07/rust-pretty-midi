use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IndexError: {0}")]
    IndexError(String),
    #[error("ValueError: {0}")]
    ValueError(String),
}
