use std::num::ParseIntError;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{map, map_res, recognize, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{many0, many0_count},
    sequence::{delimited, pair, tuple},
    IResult, InputLength, Parser,
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

define_token!(op_plus, "+", OpKind, OpKind::Add);
define_token!(op_minus, "-", OpKind, OpKind::Sub);
define_token!(op_mul, "*", OpKind, OpKind::Mul);
define_token!(op_div, "/", OpKind, OpKind::Div);
define_token!(op_pow, "^", OpKind, OpKind::Pow);
define_token!(op_assign, "=", OpKind, OpKind::Assign);
define_token!(op_not, "!", OpKind, OpKind::Not);
define_token!(op_gt, ">", OpKind, OpKind::Gt);
define_token!(op_lt, "<", OpKind, OpKind::Lt);
define_token!(op_gte, ">=", OpKind, OpKind::Gte);
define_token!(op_lte, "<=", OpKind, OpKind::Lte);
define_token!(op_eq, "==", OpKind, OpKind::Eq);
define_token!(op_not_eq, "!=", OpKind, OpKind::NotEq);
define_token!(op_comma, ",", OpKind, OpKind::Comma);

define_token!(literal_true, "true", LiteralValue, LiteralValue::Bool(true));
define_token!(
    literal_false,
    "false",
    LiteralValue,
    LiteralValue::Bool(false)
);

fn ws<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
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

fn infixr_expr<I, E, F, G, H>(op: F, lhs: G, rhs: H) -> impl FnMut(I) -> IResult<I, Expr, E>
where
    I: Clone + PartialEq + InputLength,
    E: ParseError<I>,
    F: Parser<I, OpKind, E>,
    G: Parser<I, Expr, E>,
    H: Parser<I, Expr, E>,
{
    map(tuple((lhs, many0(pair(op, rhs)))), |(lhs, rhs_list)| {
        rhs_list
            .into_iter()
            .rfold(lhs, |lhs, (op, rhs)| Expr::binop(op, lhs, rhs))
    })
}

fn infixl_expr<I, E, F, G, H>(op: F, lhs: G, rhs: H) -> impl FnMut(I) -> IResult<I, Expr, E>
where
    I: Clone + PartialEq + InputLength,
    E: ParseError<I>,
    F: Parser<I, OpKind, E>,
    G: Parser<I, Expr, E>,
    H: Parser<I, Expr, E>,
{
    map(tuple((lhs, many0(pair(op, rhs)))), |(lhs, rhs_list)| {
        rhs_list
            .into_iter()
            .fold(lhs, |lhs, (op, rhs)| Expr::binop(op, lhs, rhs))
    })
}

fn prefix_expr<'a, I, E, F, G>(op: F, rhs: G) -> impl FnMut(I) -> IResult<I, Expr, E>
where
    I: Clone + PartialEq,
    E: ParseError<I>,
    F: Parser<I, OpKind, E>,
    G: Parser<I, Expr, E>,
{
    map(tuple((op, rhs)), |(op, rhs)| Expr::unop(op, rhs))
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

fn fw_number<'a, E>(input: &'a str) -> IResult<&'a str, LiteralValue, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "number",
        map(map_res(digit1, str::parse), LiteralValue::Int32),
    )(input)
}

fn fw_literal<'a, E>(input: &'a str) -> IResult<&'a str, LiteralValue, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context("literal", alt((literal_true, literal_false, fw_number)))(input)
}

// Expr15 = literal | identifier
fn fw_expr15<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    alt((
        map(ws(fw_literal), Expr::Literal),
        map(ws(fw_identifier), Expr::Identifier),
    ))(input)
}

// Expr14 = "(" Expr ")" | Expr15
fn fw_expr14<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    alt((ws(paren(fw_expr)), fw_expr15))(input)
}

// Expr13 = Expr14 [ "^" Expr14 ]*
fn fw_expr13<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    alt((infixr_expr(ws(op_pow), fw_expr14, fw_expr13), fw_expr14))(input)
}

// Expr12 =  "-" Expr13 | "!" Expr13 | Expr13
fn fw_expr12<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    alt((
        prefix_expr(ws(op_minus), fw_expr13),
        prefix_expr(ws(op_not), fw_expr13),
        fw_expr13,
    ))(input)
}

// Expr11 = Expr12 [ ("*" | "/") Expr12 ]*
fn fw_expr11<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    infixl_expr(ws(alt((op_mul, op_div))), fw_expr12, fw_expr12)(input)
}

// Expr10 = Expr11 [ ("+" | "-") Expr11 ]*
fn fw_expr10<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    infixl_expr(ws(alt((op_plus, op_minus))), fw_expr11, fw_expr11)(input)
}

// Expr9 = Expr10 [ ("<" | ">" | "<=" | ">=" | "==" | "!=") Expr10 ]*
fn fw_expr9<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    infixl_expr(
        ws(alt((op_lt, op_gt, op_lte, op_gte, op_eq, op_not_eq))),
        fw_expr10,
        fw_expr10,
    )(input)
}

// Expr8 = Expr9 [ "," Expr9 ]*
fn fw_expr8<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    infixl_expr(
        ws(op_comma),
        fw_expr9,
        fw_expr9,
    )(input)
}

// Expr1 = Expr8 [ "=" Expr8 ]*
fn fw_expr1<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    alt((infixr_expr(ws(op_assign), fw_expr8, fw_expr8), fw_expr8))(input)
}

// Expr = Expr1
fn fw_expr<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context("expr", fw_expr1)(input)
}

fn map_nom_error<'a>(input: &'a str) -> impl FnMut(nom::Err<nom::error::VerboseError<&'a str>>) -> error::ParseError<'a> {
    move |err| {
        match err {
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
}

fn assert_parsed_full<'a>((remainder, expr): (&'a str, Expr)) -> Result<Expr, error::ParseError<'a>> {
    if str::is_empty(remainder) {
        Ok(expr)
    } else {
        Err(error::ParseError::ExpressionNotFullyParsed(remainder, expr))
    }
}

pub fn parse(input: &str) -> Result<Expr, error::ParseError> {
    fw_expr::<nom::error::VerboseError<&str>>(input)
        .map_err(map_nom_error(input))
        .and_then(assert_parsed_full)
}
