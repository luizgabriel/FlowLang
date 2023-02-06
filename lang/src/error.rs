use thiserror::Error;

use crate::ast::Expr;

#[derive(Error, Debug)]
pub enum ParseError<'a> {
    #[error("Expression not fully parsed: {1}, remaining: \"{0}\"")]
    ExpressionNotFullyParsed(&'a str, Expr),

    #[error("Invalid expression: {}", nom::error::convert_error(*.0, .1.clone()))]
    InvalidExpression(&'a str, nom::error::VerboseError<&'a str>),

    #[error("Incomplete input: {0}\n{carret:>column$}", carret = "^", column = .1)]
    IncompleteInput(&'a str, usize),

    #[error("Failed to parse expression: {}", nom::error::convert_error(*.0, .1.clone()))]
    Failure(&'a str, nom::error::VerboseError<&'a str>),
}