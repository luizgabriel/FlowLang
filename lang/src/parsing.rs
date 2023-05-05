use std::{
    num::{ParseFloatError, ParseIntError},
    str::FromStr,
};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of},
    combinator::{map, map_res, opt, recognize, value, verify},
    error::{context, ContextError, FromExternalError},
    multi::{many0_count, many1},
    sequence::{delimited, pair, separated_pair, tuple},
    IResult, Parser,
};
use thiserror::Error;

use crate::string::parse_string;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub(crate) String);

impl Ident {
    pub fn new(name: &str) -> Self {
        Ident(name.to_string())
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Ident::new(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamsList {
    params: Vec<Ident>,
}

impl ParamsList {
    pub fn new(params: Vec<Ident>) -> Self {
        ParamsList { params }
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn get(&self, index: usize) -> Option<&Ident> {
        self.params.get(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Ident> {
        self.params.iter()
    }

    pub fn split_first(&self) -> Option<(&Ident, &[Ident])> {
        self.params.split_first()
    }
}

impl IntoIterator for ParamsList {
    type Item = Ident;
    type IntoIter = <Vec<Ident> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.params.into_iter()
    }
}

impl FromIterator<Ident> for ParamsList {
    fn from_iter<T: IntoIterator<Item = Ident>>(iter: T) -> Self {
        ParamsList::new(iter.into_iter().collect())
    }
}

impl std::fmt::Display for ParamsList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            self.iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

#[macro_export]
macro_rules! params {
    ($($param:ident),+) => {
        ParamsList::new(vec![$(Ident::new(stringify!($param))),*])
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Unit,
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
    Identifier(Ident),
    ConstantDefinition {
        name: Ident,
        expr: Box<Expr>,
    },
    FunctionApplication(Box<Expr>, Box<Expr>),
    FunctionDefinition {
        name: Ident,
        params: ParamsList,
        body: Box<Expr>,
    },
    Lambda {
        params: ParamsList,
        body: Box<Expr>,
    },
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
}

impl Expr {
    pub fn ident(name: &str) -> Expr {
        Expr::Identifier(Ident::new(name))
    }

    pub fn constdef(name: Ident, value: Expr) -> Expr {
        Expr::ConstantDefinition {
            name,
            expr: Box::new(value),
        }
    }

    pub fn fndef(name: Ident, params: ParamsList, body: Expr) -> Expr {
        Expr::FunctionDefinition {
            name,
            params,
            body: Box::new(body),
        }
    }

    pub fn fnapp(func: Expr, arg: Expr) -> Expr {
        Expr::FunctionApplication(Box::new(func), Box::new(arg))
    }

    pub fn lambda(params: ParamsList, body: Expr) -> Expr {
        Expr::Lambda {
            params,
            body: Box::new(body),
        }
    }

    pub fn ife(condition: Expr, then: Expr, otherwise: Expr) -> Expr {
        Expr::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    }
}

#[derive(Error, Debug)]
pub enum ParseError<'a> {
    #[error("Expression not fully parsed: {0}, Remaining: \"{1}\"")]
    ExpressionNotFullyParsed(Expr, &'a str),

    #[error("Invalid expression: {}", nom::error::convert_error(*.0, .1.clone()))]
    InvalidExpression(&'a str, nom::error::VerboseError<&'a str>),

    #[error("Incomplete input: {0}\n{carret:>column$}", carret = "^", column = .1)]
    IncompleteInput(&'a str, usize),

    #[error("Failed to parse expression: {}", nom::error::convert_error(*.0, .1.clone()))]
    Failure(&'a str, nom::error::VerboseError<&'a str>),
}

fn ws0<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    E: nom::error::ParseError<&'a str>,
{
    delimited(multispace0, inner, multispace0)
}

fn paren<'a, O, E, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    E: nom::error::ParseError<&'a str>,
{
    delimited(tag("("), inner, tag(")"))
}

fn blacklist<'o, I, O, E, F>(
    parser: F,
    blacklist: &'o [O],
) -> impl FnMut(I) -> IResult<I, O, E> + 'o
where
    I: Clone + 'o,
    F: Parser<I, O, E> + 'o,
    E: nom::error::ParseError<I> + 'o,
    O: std::cmp::PartialEq,
{
    verify(parser, |result| !blacklist.contains(result))
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

    context("identifier", map(identifier_except_keywords, Ident::new))(input)
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

fn fw_float<'a, T, E>(input: &'a str) -> IResult<&'a str, T, E>
where
    T: FromStr<Err = ParseFloatError>,
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    alt((ws0(paren(fw_expr)), fw_expr15))(input)
}

// Function Application
// Expr13 = Expr14 Expr14 Expr14 ...
fn fw_expr13<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: nom::error::ParseError<&'a str>
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
    E: nom::error::ParseError<&'a str>
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
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
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
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
fn fw_expr<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: nom::error::ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
{
    context("expr", fw_expr8)(input)
}

fn map_nom_error<'a>(
    input: &'a str,
) -> impl FnMut(nom::Err<nom::error::VerboseError<&'a str>>) -> ParseError<'a> {
    move |err| match err {
        nom::Err::Incomplete(needed) => ParseError::IncompleteInput(
            input,
            match needed {
                nom::Needed::Unknown => 0,
                nom::Needed::Size(n) => n.get(),
            },
        ),
        nom::Err::Error(e) => ParseError::InvalidExpression(input, e),
        nom::Err::Failure(e) => ParseError::Failure(input, e),
    }
}

fn assert_parsed_full<'a>((remainder, expr): (&'a str, Expr)) -> Result<Expr, ParseError<'a>> {
    if str::is_empty(remainder) {
        Ok(expr)
    } else {
        Err(ParseError::ExpressionNotFullyParsed(expr, remainder))
    }
}

pub fn parse(input: &str) -> Result<Expr, ParseError> {
    fw_expr::<nom::error::VerboseError<&str>>(input)
        .map_err(map_nom_error(input))
        .and_then(assert_parsed_full)
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}
