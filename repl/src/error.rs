use lang::{evaluation::error::EvalError, parsing::error::ParseError};
use rustyline::error::ReadlineError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum REPLError {
    #[error("Readline error: {0}")]
    ReadlineError(String),

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Incomplete input")]
    IncompleteInput,

    #[error("Evaluation error: {0}")]
    EvaluationError(String),
}

impl<'a> Into<REPLError> for ParseError {
    fn into(self) -> REPLError {
        match self {
            ParseError::IncompleteInput => REPLError::IncompleteInput,
            ParseError::ParseError(e) => REPLError::ParseError(e),
        }
    }
}

impl Into<REPLError> for EvalError {
    fn into(self) -> REPLError {
        REPLError::EvaluationError(self.to_string())
    }
}

impl Into<REPLError> for ReadlineError {
    fn into(self) -> REPLError {
        match self {
            ReadlineError::Interrupted => REPLError::ReadlineError("CTRL-C".to_string()),
            ReadlineError::Eof => REPLError::ReadlineError("CTRL-D".to_string()),
            err => REPLError::ReadlineError(err.to_string()),
        }
    }
}
