use std::num::{ParseFloatError, ParseIntError};

use nom::error::{ContextError, FromExternalError};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expression not fully parsed. Remaining: \"{0}\"")]
    NotFullyParsed(String),

    #[error("Failed to parse expression: {0}")]
    Failed(String),
}

pub trait FwError<I>:
nom::error::ParseError<I>
+ ContextError<I>
+ FromExternalError<I, ParseIntError>
+ FromExternalError<I, ParseFloatError>
{}

impl<I, T> FwError<I> for T where
    T: nom::error::ParseError<I>
    + ContextError<I>
    + FromExternalError<I, ParseIntError>
    + FromExternalError<I, ParseFloatError>
{}