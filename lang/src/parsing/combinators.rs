use std::{
    num::{ParseFloatError, ParseIntError},
    str::FromStr,
};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, digit1, multispace0, multispace1, one_of, space0,
    },
    combinator::{map, map_res, opt, recognize, value, verify},
    error::{context, ContextError, FromExternalError},
    multi::{many0_count, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    AsChar, Compare, IResult, InputLength, InputTake, InputTakeAtPosition, Parser,
};

use super::{
    ast::{Expr, Ident, Module, ParamsList},
    error::FwError,
    string::parse_string,
};

fn ws0<'a, I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E>,
    E: nom::error::ParseError<I>,
{
    preceded(space0, inner)
}

fn paren<'a, I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTake + InputLength + Compare<&'static str>,
    F: Parser<I, O, E>,
    E: nom::error::ParseError<I>,
{
    delimited(tag("("), inner, tag(")"))
}

fn blacklist<'o, I, O, B, K, E, F>(
    parser: F,
    blacklist: B,
) -> impl FnMut(I) -> IResult<I, O, E> + 'o
where
    I: Clone + 'o,
    F: Parser<I, O, E> + 'o,
    E: nom::error::ParseError<I> + 'o,
    O: 'o,
    B: IntoIterator<Item = &'o K> + Clone + 'o,
    K: std::cmp::PartialEq<O> + 'o,
{
    verify(parser, move |result| {
        !blacklist
            .clone()
            .into_iter()
            .find(|&k| k == result)
            .is_some()
    })
}

fn fw_identifier<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
    E: nom::error::ParseError<&'a str> + ContextError<&'a str>,
{
    let identifier = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ));

    //Dont allow keywords "if", "then" and "else"
    const KEYWORDS: [&'static str; 3] = ["if", "then", "else"];
    let identifier_except_keywords = blacklist(identifier, &KEYWORDS);

    context(
        "identifier",
        map(identifier_except_keywords, |ident: &'a str| {
            Ident::new(ident.into())
        }),
    )(input)
}

fn fw_operator<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
    E: nom::error::ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "operator",
        map(recognize(many1(one_of("!$%^&*-=+<>.~\\/|:"))), Ident::new),
    )(input)
}

fn fw_nat<'a, T, E>(input: &'a str) -> IResult<&'a str, T, E>
where
    T: FromStr<Err = ParseIntError>,
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "nat",
        map_res(recognize(pair(opt(tag("-")), digit1)), str::parse),
    )(input)
}

fn fw_float<'a, O, E>(input: &'a str) -> IResult<&'a str, O, E>
where
    O: FromStr<Err = ParseFloatError>,
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>,
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

fn fw_literal<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    let parse_unit = value(Expr::Unit, tag("()"));
    let parse_true = value(Expr::Bool(true), tag("true"));
    let parse_false = value(Expr::Bool(false), tag("false"));
    let parse_float = map(fw_float, Expr::Float32);
    let parse_nat = map(fw_nat, Expr::Int32);

    context(
        "literal",
        alt((
            parse_unit,
            parse_true,
            parse_false,
            parse_float,
            parse_nat,
            map(parse_string, Expr::String),
        )),
    )(input)
}

// Expr15 = literal | identifier | (operator)
fn fw_expr15<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((
        ws0(fw_literal),
        map(ws0(fw_identifier), Expr::Identifier),
        map(ws0(paren(fw_operator)), Expr::Identifier),
    ))(input)
}

// Expr14 = "(" Expr ")" | Expr15
fn fw_expr14<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((ws0(paren(fw_expr)), fw_expr15))(input)
}

// Function Application
// Expr13 = Expr14 Expr14 Expr14 ...
fn fw_expr13<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
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
    E: FwError<&'a str>,
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

fn fw_param_list<'a, E>(input: &'a str) -> IResult<&'a str, ParamsList, E>
where
    E: nom::error::ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "parameter list",
        map(many1(ws0(fw_identifier)), ParamsList::new),
    )(input)
}

// (Lambda Expressions)
// Expr11 = (arg1 arg2 ... argN -> Expr12)
fn fw_expr11<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    let lambda = context(
        "lambda expression",
        map(
            tuple((fw_param_list, ws0(tag("->")), fw_expr12)),
            |(args, _, body)| Expr::lambda(args, body),
        ),
    );

    alt((lambda, fw_expr12))(input)
}

// If Expression
// Expr10 = if Expr11 then Expr11 else Expr11
fn fw_expr10<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    let if_expr = context(
        "if expression",
        map(
            tuple((
                ws0(tag("if")),
                fw_expr11,
                ws0(tag("then")),
                fw_expr11,
                ws0(tag("else")),
                fw_expr11,
            )),
            |(_, condition, _, then, _, otherwise)| Expr::ife(condition, then, otherwise),
        ),
    );

    alt((if_expr, fw_expr11))(input)
}

// Function Definition
// Expr9 = ident arg1 arg2 ... argN = Expr10
// Expr9 = (operator) arg1 arg2 ... argnN = Expr10
fn fw_expr9<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    let func_def = context(
        "function definition",
        map(
            tuple((
                ws0(alt((fw_identifier, paren(ws0(fw_operator))))),
                fw_param_list,
                ws0(tag("=")),
                fw_expr10,
            )),
            |(name, args, _, body)| Expr::fndef(name, args, body),
        ),
    );

    alt((func_def, fw_expr10))(input)
}

// Constant Variable Definition
// ident = Expr9
fn fw_expr8<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    let const_def = context(
        "constant variable definition",
        map(
            tuple((ws0(fw_identifier), ws0(tag("=")), fw_expr9)),
            |(name, _, expr)| Expr::constdef(name, expr),
        ),
    );

    alt((const_def, fw_expr9))(input)
}

// Expr0 = Expr8
pub fn fw_expr<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    context("expr", fw_expr8)(input)
}

pub fn fw_module<'a, E>(input: &'a str) -> IResult<&'a str, Module, E>
where
    E: FwError<&'a str>,
{
    let expr_block = delimited(
        multispace0,
        separated_list0(multispace1, fw_expr),
        multispace0,
    );

    context("module", map(expr_block, Module::new))(input)
}
