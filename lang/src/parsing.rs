use std::{
    num::{ParseFloatError, ParseIntError},
    str::FromStr,
};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of},
    combinator::{map, map_res, opt, recognize, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{many0_count, many1},
    sequence::{delimited, pair, separated_pair, tuple},
    IResult, Parser,
};

use crate::{ast::*, error};

macro_rules! define_token {
    ($name:ident, $tag:expr, $out:ty, $cto:expr) => {
        fn $name<'a, E>(input: &'a str) -> IResult<&'a str, $out, E>
        where
            E: ParseError<&'a str>,
        {
            value($cto, tag($tag))(input)
        }
    };
}

define_token!(literal_unit, "()", Value, Value::Unit());
define_token!(literal_true, "true", Value, Value::Bool(true));
define_token!(literal_false, "false", Value, Value::Bool(false));

fn ws0<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    E: ParseError<&'a str>,
{
    delimited(multispace0, inner, multispace0)
}

fn paren<'a, O, E, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    E: ParseError<&'a str>,
{
    delimited(tag("("), inner, tag(")"))
}

fn fw_identifier<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "identifier",
        map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0_count(alt((alphanumeric1, tag("_")))),
            )),
            Ident::new,
        ),
    )(input)
}

fn fw_operator<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "operator",
        map(recognize(many1(one_of("!$%^&*-=+<>.~\\/|:"))), Ident::new),
    )(input)
}

fn fw_nat<'a, T, E>(input: &'a str) -> IResult<&'a str, T, E>
where
    T: FromStr<Err = ParseIntError>,
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "nat",
        map_res(recognize(pair(opt(tag("-")), digit1)), str::parse),
    )(input)
}

fn fw_float<'a, T, E>(input: &'a str) -> IResult<&'a str, T, E>
where
    T: FromStr<Err = ParseFloatError>,
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseFloatError>,
{
    context(
        "float",
        map_res(
            recognize(pair(
                opt(tag("-")),
                separated_pair(digit1, tag("."), digit1),
            )),
            str::parse,
        ),
    )(input)
}

fn fw_number<'a, E>(input: &'a str) -> IResult<&'a str, Value, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    context(
        "number",
        alt((
            map(fw_float, Value::Float32), //
            map(fw_nat, Value::Int32),
        )),
    )(input)
}

fn fw_literal<'a, E>(input: &'a str) -> IResult<&'a str, Value, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    context("literal", alt((literal_true, literal_false, fw_number)))(input)
}

// Expr15 = literal | identifier | (operator)
fn fw_expr15<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    alt((
        map(ws0(literal_unit), Expr::Literal),
        map(ws0(fw_literal), Expr::Literal),
        map(ws0(fw_identifier), Expr::Identifier),
        map(ws0(paren(fw_operator)), Expr::Identifier),
    ))(input)
}

// Expr14 = "(" Expr ")" | Expr15
fn fw_expr14<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    alt((ws0(paren(fw_expr)), fw_expr15))(input)
}

// Expr14 Expr14 Expr14 ...
// Expr14 operator Expr14 ...
// (operator) Expr14 Expr14 ...
fn fw_expr13<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    let fn_app = context(
        "function application",
        map(tuple((fw_expr14, many1(fw_expr14))), |(head, tail)| {
            tail.into_iter().fold(head, Expr::fnapp)
        }),
    );

    alt((fn_app, fw_expr14))(input)
}

// Infix Operator Function Application
// Expr12 = Expr13 [ "Operator" Expr13 ]*
fn fw_expr12<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    let infix_op_fn_app = context(
        "infix operator function application",
        map(
            tuple((fw_expr13, many1(pair(ws0(fw_operator), fw_expr13)))),
            |(head, tail)| {
                tail.into_iter().fold(head, |acc, (op, rhs)| {
                    Expr::fnapp(Expr::fnapp(Expr::Identifier(op), acc), rhs)
                })
            },
        ),
    );

    alt((infix_op_fn_app, fw_expr13))(input)
}

// (Function Definition)
// ident arg1 arg2 ... argN = expr13
fn fw_expr11<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    let func_def = context(
        "function definition",
        map(
            tuple((
                ws0(alt((fw_identifier, paren(ws0(fw_operator))))),
                many1(ws0(fw_identifier)),
                ws0(tag("=")),
                fw_expr,
            )),
            |(name, args, _, body)| Expr::fndef(name, args, body),
        ),
    );

    alt((func_def, fw_expr12))(input)
}

// (Lambda Expressions)
// (x y -> x + y)
fn fw_expr10<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    let lambda = context(
        "lambda expression",
        map(
            tuple((
                ws0(tag("(")),
                many1(ws0(fw_identifier)),
                ws0(tag("->")),
                fw_expr,
                ws0(tag(")")),
            )),
            |(_, args, _, body, _)| Expr::lambda(args, body),
        ),
    );

    alt((lambda, fw_expr11))(input)
}

// Constant Variable Definition
// ident = expr10
fn fw_expr9<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    let const_def = context(
        "constant variable definition",
        map(
            tuple((ws0(fw_identifier), ws0(tag("=")), fw_expr)),
            |(name, _, expr)| Expr::constdef(name, expr),
        ),
    );

    alt((const_def, fw_expr10))(input)
}

// Expr = Expr9
fn fw_expr<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    context("expr", fw_expr9)(input)
}

fn map_nom_error<'a>(
    input: &'a str,
) -> impl FnMut(nom::Err<nom::error::VerboseError<&'a str>>) -> error::ParseError<'a> {
    move |err| match err {
        nom::Err::Incomplete(needed) => error::ParseError::IncompleteInput(
            input,
            match needed {
                nom::Needed::Unknown => 0,
                nom::Needed::Size(n) => n.get(),
            },
        ),
        nom::Err::Error(e) => error::ParseError::InvalidExpression(input, e),
        nom::Err::Failure(e) => error::ParseError::Failure(input, e),
    }
}

fn assert_parsed_full<'a>(
    (remainder, expr): (&'a str, Expr),
) -> Result<Expr, error::ParseError<'a>> {
    if str::is_empty(remainder) {
        Ok(expr)
    } else {
        Err(error::ParseError::ExpressionNotFullyParsed(remainder))
    }
}

pub fn parse(input: &str) -> Result<Expr, error::ParseError> {
    fw_expr::<nom::error::VerboseError<&str>>(input)
        .map_err(map_nom_error(input))
        .and_then(assert_parsed_full)
}
