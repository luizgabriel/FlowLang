use thiserror::Error;

use crate::evaluation::data::Value;
use crate::parsing::data::Ident;

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unbound variable: {0}")]
    UnboundIdentifier(Ident),

    #[error("Not a function: {0}")]
    NotAFunction(Value),

    #[error("Invalid type: expected {expected} got {value}")]
    InvalidType {
        value: Value,
        expected: &'static str,
    },

    #[error("Division by zero")]
    DivideByZero,

    #[error("Operation Overflow")]
    Overflow,
}