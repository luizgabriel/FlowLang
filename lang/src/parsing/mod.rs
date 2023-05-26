mod combinators;
pub mod data;
mod display;
pub mod error;
mod string;

use crate::parsing::combinators::{fw_expr, fw_module};
use crate::parsing::data::{Expr, Module};
use crate::parsing::error::ParseError;
use nom::error::convert_error;

fn to_parse_error(
    input: &str,
) -> impl FnOnce(nom::Err<nom::error::VerboseError<&str>>) -> ParseError + '_ {
    move |error| match error {
        nom::Err::Incomplete(needed) => panic!("Incomplete Input: {:?}", needed),
        nom::Err::Failure(e) | nom::Err::Error(e) => ParseError::Failed(convert_error(input, e)),
    }
}

fn assert_parsed_fully<T>(input: &str, value: T) -> Result<T, ParseError> {
    if input.is_empty() {
        Ok(value)
    } else {
        Err(ParseError::NotFullyParsed(input.to_owned()))
    }
}

pub fn parse_module(input: &str) -> Result<Module, ParseError> {
    fw_module::<nom::error::VerboseError<_>>(input)
        .map_err(to_parse_error(input))
        .and_then(|(i, o)| assert_parsed_fully(i, o))
}

pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    fw_expr::<nom::error::VerboseError<_>>(input)
        .map_err(to_parse_error(input))
        .and_then(|(i, o)| assert_parsed_fully(i, o))
}
