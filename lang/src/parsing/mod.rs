use nom::error::convert_error;

use crate::parsing::combinators::{expr, program, statement};
pub use crate::parsing::data::{Expr, Ident, ParamsList, Program, Statement};
pub use crate::parsing::error::ParseError;

mod combinators;
pub mod data;
mod display;
pub mod error;
mod string;

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

fn full_parse<'a, F, O, >(mut parser: F) -> impl FnMut(&'a str) -> Result<O, ParseError>
    where F: nom::Parser<&'a str, O, nom::error::VerboseError<&'a str>>,
{
    move |input| {
        parser.parse(input)
            .map_err(to_parse_error(input))
            .and_then(|(i, o)| assert_parsed_fully(i, o))
    }
}

pub fn parse_program(input: &str) -> Result<Program, ParseError> {
    full_parse(program)(input)
}

pub fn parse_statement(input: &str) -> Result<Statement, ParseError> {
    full_parse(statement)(input)
}

pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    full_parse(expr)(input)
}
