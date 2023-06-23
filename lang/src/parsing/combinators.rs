use std::num::{ParseFloatError, ParseIntError};
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{
    alpha1, alphanumeric1, digit1, multispace0, multispace1, one_of, space0,
};
use nom::combinator::{map, map_res, opt, recognize, value, verify};
use nom::error::{context, ContextError, FromExternalError};
use nom::multi::{many0_count, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::{AsChar, Compare, IResult, InputLength, InputTake, InputTakeAtPosition};

use crate::parsing::data::{Expr, Ident, ParamsList, Program, Statement};
use crate::parsing::error::FwError;
use crate::parsing::string::parse_string;

fn ws0<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, O, E>,
    E: nom::error::ParseError<I> + ContextError<I>,
{
    context("ws0", preceded(space0, inner))
}

fn paren<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTake + InputLength + Compare<&'static str> + Clone,
    F: nom::Parser<I, O, E>,
    E: nom::error::ParseError<I> + ContextError<I>,
{
    context("parenthesis", delimited(tag("("), inner, tag(")")))
}

fn blacklist<'o, I, O, B, K, E, F>(
    parser: F,
    blacklist: B,
) -> impl FnMut(I) -> IResult<I, O, E> + 'o
where
    I: Clone + 'o,
    F: nom::Parser<I, O, E> + 'o,
    E: nom::error::ParseError<I> + ContextError<I> + 'o,
    O: 'o,
    B: IntoIterator<Item = &'o K> + Clone + 'o,
    K: PartialEq<O> + 'o,
{
    context(
        "blacklist",
        verify(parser, move |result| {
            !blacklist.clone().into_iter().any(|k| k == result)
        }),
    )
}

fn identifier<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
    E: nom::error::ParseError<&'a str> + ContextError<&'a str>,
{
    let identifier = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ));

    //Dont allow these keywords
    const KEYWORDS: [&str; 4] = ["let", "if", "then", "else"];
    let identifier_except_keywords = blacklist(identifier, &KEYWORDS);

    context(
        "identifier",
        map(identifier_except_keywords, |ident: &'a str| {
            Ident::name(ident)
        }),
    )(input)
}

fn operator<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
    E: nom::error::ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "operator",
        map(recognize(many1(one_of("!$%^&*-=+<>.~\\/|:"))), Ident::op),
    )(input)
}

fn natural_number<'a, T, E>(input: &'a str) -> IResult<&'a str, T, E>
where
    T: FromStr<Err = ParseIntError>,
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "natural_number",
        map_res(recognize(pair(opt(tag("-")), digit1)), str::parse),
    )(input)
}

fn floating_number<'a, O, E>(input: &'a str) -> IResult<&'a str, O, E>
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

fn literal<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    let parse_unit = value(Expr::Unit, tag("()"));
    let parse_true = value(Expr::Bool(true), tag("true"));
    let parse_false = value(Expr::Bool(false), tag("false"));
    let parse_float = map(floating_number, Expr::Float32);
    let parse_nat = map(natural_number, Expr::Int32);

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

// literal | identifier | (operator)
fn expr_p15<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    context(
        "literal or identifier or (operator)",
        alt((
            ws0(literal),
            map(ws0(identifier), Expr::Identifier),
            map(ws0(paren(operator)), Expr::Identifier),
        )),
    )(input)
}

fn expr_p14<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((ws0(paren(expr)), expr_p15))(input)
}

// Function Application
// Expr14 Expr14 Expr14 ...
fn expr_p13<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((
        context(
            "function application",
            map(tuple((expr_p14, many1(expr_p14))), |(head, tail)| {
                tail.into_iter().fold(head, Expr::fnapp)
            }),
        ),
        expr_p14,
    ))(input)
}

// Infix Operator Function Application
// Expr13 [ "Operator" Expr13 ]*
fn expr_p12<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((
        context(
            "infix operator function application",
            map(
                tuple((expr_p13, many1(pair(ws0(operator), expr_p13)))),
                |(head, tail)| {
                    tail.into_iter().fold(head, |acc, (op, rhs)| {
                        Expr::fnapp(Expr::fnapp(Expr::Identifier(op), acc), rhs)
                    })
                },
            ),
        ),
        expr_p13,
    ))(input)
}

// Params List
// arg1 arg2 ... argN
fn param_list<'a, E>(input: &'a str) -> IResult<&'a str, ParamsList, E>
where
    E: nom::error::ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "parameter list",
        map(many1(ws0(identifier)), ParamsList::new),
    )(input)
}

// (Lambda Expressions)
// arg1 arg2 ... argN -> Expr12
fn expr_p11<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((
        context(
            "lambda expression",
            map(
                tuple((param_list, ws0(tag("->")), expr_p12)),
                |(args, _, body)| Expr::lambda(args, body),
            ),
        ),
        expr_p12,
    ))(input)
}

// If Expression
// if Expr11 then Expr11 else Expr11
fn expr_p10<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    alt((
        context(
            "if expression",
            map(
                tuple((
                    ws0(tag("if")),
                    expr_p11,
                    ws0(tag("then")),
                    expr_p11,
                    ws0(tag("else")),
                    expr_p11,
                )),
                |(_, condition, _, then, _, otherwise)| Expr::ife(condition, then, otherwise),
            ),
        ),
        expr_p11,
    ))(input)
}

// Root Expression
pub fn expr<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FwError<&'a str>,
{
    context("expr", expr_p10)(input)
}

// Constant Definition
// ident = Expr
fn const_def<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: FwError<&'a str>,
{
    context(
        "constant variable definition",
        map(
            tuple((ws0(identifier), ws0(tag("=")), expr)),
            |(name, _, expr)| Statement::constant(name, expr),
        ),
    )(input)
}

// Function Definition
// ident arg1 arg2 ... argN = Expr
// (operator) arg1 arg2 ... argnN = Expr
fn func_def<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: FwError<&'a str>,
{
    context(
        "function definition",
        map(
            tuple((
                ws0(alt((identifier, paren(ws0(operator))))),
                param_list,
                ws0(tag("=")),
                statement,
            )),
            |(name, args, _, body)| Statement::function(name, args, body),
        ),
    )(input)
}

fn let_block<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: FwError<&'a str>,
{
    context(
        "statement let block",
        map(
            delimited(
                tag("{"),
                separated_list1(tag(";"), statement),
                preceded(multispace0, tag("}")),
            ),
            Statement::block,
        ),
    )(input)
}

pub fn statement<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: FwError<&'a str>,
{
    context(
        "statement",
        preceded(
            multispace0,
            alt((let_block, const_def, func_def, map(expr, Statement::expr))),
        ),
    )(input)
}

pub fn program<'a, E>(input: &'a str) -> IResult<&'a str, Program, E>
where
    E: FwError<&'a str>,
{
    let expr_block = delimited(
        multispace0,
        separated_list0(multispace1, statement),
        multispace0,
    );

    context("module", map(expr_block, Program::new))(input)
}
