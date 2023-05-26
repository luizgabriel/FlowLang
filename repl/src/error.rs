use lang::{evaluation::EvalError, parsing::ParseError};
use rustyline::error::ReadlineError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum REPLError {
    #[error("Readline error: {0}")]
    Readline(String),

    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Evaluation error: {0}")]
    Evaluation(String),
}

impl<'a> From<ParseError<'a>> for REPLError {
    fn from(val: ParseError<'a>) -> Self {
        REPLError::Parse(val.to_string())
    }
}

impl From<EvalError> for REPLError {
    fn from(val: EvalError) -> Self {
        REPLError::Evaluation(val.to_string())
    }
}

impl From<ReadlineError> for REPLError {
    fn from(val: ReadlineError) -> Self {
        match val {
            ReadlineError::Interrupted => REPLError::Readline("CTRL-C".to_string()),
            ReadlineError::Eof => REPLError::Readline("CTRL-D".to_string()),
            err => REPLError::Readline(err.to_string()),
        }
    }
}
