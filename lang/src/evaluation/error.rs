use std::io;

use thiserror::Error;

use crate::evaluation::data::Value;
use crate::parsing::data::{Ident, ModuleName};
use crate::parsing::ParseError;

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unbound variable: {0}")]
    UnboundIdentifier(Ident),

    #[error("Could not parse module: {0}\n{1}")]
    InvalidModule(ModuleName, ParseError),

    #[error("Could not read module: {0}\n{1}")]
    CouldNotReadModule(ModuleName, io::Error),

    #[error("Not a function: {0}")]
    NotAFunction(Value),

    #[error("Invalid type: expected {expected} got {value:?}")]
    InvalidType {
        value: Value,
        expected: &'static str,
    },

    #[error("Division by zero")]
    DivideByZero,

    #[error("Operation Overflow")]
    Overflow,
}
