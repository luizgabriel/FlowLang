use std::num::{ParseFloatError, ParseIntError};

use nom::error::{ContextError, FromExternalError};

pub trait FwError<I>:
    nom::error::ParseError<I>
    + ContextError<I>
    + FromExternalError<I, ParseIntError>
    + FromExternalError<I, ParseFloatError>
{
}

impl<I, T> FwError<I> for T where
    T: nom::error::ParseError<I>
        + ContextError<I>
        + FromExternalError<I, ParseIntError>
        + FromExternalError<I, ParseFloatError>
{
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ParseError(String),
    IncompleteInput,
}
