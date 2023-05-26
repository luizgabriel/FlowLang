use rustyline::error::ReadlineError;
use thiserror::Error;
use lang::evaluation::error::EvalError;
use lang::parsing::error::ParseError;

#[derive(Error, Debug)]
pub enum REPLError {
    #[error("Readline error: {0}")]
    Readline(String),

    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Evaluation error: {0}")]
    Evaluation(String),
}

impl From<ParseError> for REPLError {
    fn from(val: ParseError) -> Self {
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
