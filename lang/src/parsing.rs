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
    error::{context, convert_error, ContextError, FromExternalError},
    multi::{many0_count, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    AsChar, Compare, IResult, InputLength, InputTake, InputTakeAtPosition,
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

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
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

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
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

    pub fn split_first(&self) -> Option<(Ident, ParamsList)> {
        let (head, tail) = self.params.split_first()?;
        Some((head.clone(), Self::new(tail.to_vec())))
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

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    expressions: Vec<Expr>,
}

impl Module {
    pub fn new(expressions: Vec<Expr>) -> Self {
        Module { expressions }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Expr> {
        self.expressions.iter()
    }
}

impl IntoIterator for Module {
    type Item = Expr;
    type IntoIter = <Vec<Expr> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.expressions.into_iter()
    }
}

impl FromIterator<Expr> for Module {
    fn from_iter<T: IntoIterator<Item = Expr>>(iter: T) -> Self {
        Module::new(iter.into_iter().collect())
    }
}

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
{
}

impl<I, T> FwError<I> for T where
    T: nom::error::ParseError<I>
        + ContextError<I>
        + FromExternalError<I, ParseIntError>
        + FromExternalError<I, ParseFloatError>
{
}

fn ws0<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, O, E>,
    E: nom::error::ParseError<I>,
{
    preceded(space0, inner)
}

fn paren<I, O, E, F>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTake + InputLength + Compare<&'static str>,
    F: nom::Parser<I, O, E>,
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
    F: nom::Parser<I, O, E> + 'o,
    E: nom::error::ParseError<I> + 'o,
    O: 'o,
    B: IntoIterator<Item = &'o K> + Clone + 'o,
    K: std::cmp::PartialEq<O> + 'o,
{
    verify(parser, move |result| {
        !blacklist.clone().into_iter().any(|k| k == result)
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
    const KEYWORDS: [&str; 3] = ["if", "then", "else"];
    let identifier_except_keywords = blacklist(identifier, &KEYWORDS);

    context(
        "identifier",
        map(identifier_except_keywords, |ident: &'a str| {
            Ident::new(ident)
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

fn to_parse_error(
    input: &str,
) -> impl FnOnce(nom::Err<nom::error::VerboseError<&str>>) -> ParseError + '_ {
    move |error| match error {
        nom::Err::Incomplete(needed) => panic!("Incomplete Input: {:?}", needed),
        nom::Err::Failure(e) | nom::Err::Error(e) => ParseError::Failed(convert_error(input, e)),
    }
}

fn assert_parsed_fully<T>(input: &str, value: T) -> Result<T, ParseError> {
    if !input.is_empty() {
        Err(ParseError::NotFullyParsed(input.to_owned()))
    }

    Ok(value)
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
