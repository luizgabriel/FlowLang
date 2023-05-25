use self::combinators::{fw_expr, fw_module};
use nom::error::convert_error;

pub mod ast;
mod combinators;
pub mod display;
pub mod error;
mod string;

fn to_parse_error<'a>(
    input: &'a str,
) -> impl FnOnce(nom::Err<nom::error::VerboseError<&'a str>>) -> error::ParseError {
    move |error| match error {
        nom::Err::Incomplete(_) => error::ParseError::IncompleteInput,
        nom::Err::Failure(e) | nom::Err::Error(e) => {
            error::ParseError::ParseError(convert_error(input, e))
        }
    }
}

pub fn parse_module(input: &str) -> Result<ast::Module, error::ParseError> {
    fw_module::<nom::error::VerboseError<_>>(input)
        .map_err(to_parse_error(input))
        .map(|(_, o)| o)
}

pub fn parse_expr(input: &str) -> Result<ast::Expr, error::ParseError> {
    fw_expr::<nom::error::VerboseError<_>>(input)
        .map_err(to_parse_error(input))
        .map(|(_, o)| o)
}
