use thiserror::Error;

#[derive(Error, Debug)]
pub enum REPLError {
    #[error("Readline error: {0}")]
    ReadlineError(String),

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Evaluation error: {0}")]
    EvaluationError(String),
}

impl<'a> Into<REPLError> for lang::error::ParseError<'a> {
    fn into(self) -> REPLError {
        REPLError::ParseError(self.to_string())
    }
}
