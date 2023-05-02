use thiserror::Error;

use crate::ast::{Expr, Ident, Value};

#[derive(Error, Debug)]
pub enum ParseError<'a> {
    #[error("Expression not fully parsed: {0}, Remaining: \"{1}\"")]
    ExpressionNotFullyParsed(Expr, &'a str),

    #[error("Invalid expression: {}", nom::error::convert_error(*.0, .1.clone()))]
    InvalidExpression(&'a str, nom::error::VerboseError<&'a str>),

    #[error("Incomplete input: {0}\n{carret:>column$}", carret = "^", column = .1)]
    IncompleteInput(&'a str, usize),

    #[error("Failed to parse expression: {}", nom::error::convert_error(*.0, .1.clone()))]
    Failure(&'a str, nom::error::VerboseError<&'a str>),
}

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
