#![feature(assert_matches)]

use std::num::ParseIntError;

use ast::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{map, map_res, recognize, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::{many0_count, many1},
    sequence::{delimited, pair, tuple},
    IResult, InputLength, Parser,
};

mod ast;

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
//define_token!(op_eq, "=", OpKind, OpKind::Eq);
//define_token!(op_not, "!", OpKind, OpKind::Not);
define_token!(op_gt, ">", OpKind, OpKind::Gt);
define_token!(op_lt, "<", OpKind, OpKind::Lt);
define_token!(op_gte, ">=", OpKind, OpKind::Gte);
define_token!(op_lte, "<=", OpKind, OpKind::Lte);

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
    I: Clone + PartialEq,
    E: ParseError<I>,
    F: Parser<I, OpKind, E>,
    G: Parser<I, Expr, E>,
    H: Parser<I, Expr, E>,
{
    map(tuple((lhs, op, rhs)), |(lhs, op, rhs)| {
        Expr::binop(op, lhs, rhs)
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
    map(tuple((lhs, many1(pair(op, rhs)))), |(lhs, rhs_list)| {
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

fn fw_expr15<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "expr15",
        alt((
            map(ws(fw_literal), Expr::Literal),
            map(ws(fw_identifier), Expr::Identifier),
        )),
    )(input)
}

fn fw_expr14<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context("expr14", alt((paren(fw_expr), fw_expr15)))(input)
}

fn fw_expr13<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "expr13",
        alt((infixr_expr(ws(op_pow), fw_expr14, fw_expr13), fw_expr14)),
    )(input)
}

fn fw_expr12<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "expr12",
        alt((
            infixl_expr(ws(alt((op_mul, op_div))), fw_expr13, fw_expr13),
            fw_expr13,
        )),
    )(input)
}

fn fw_expr11<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "expr11",
        alt((
            infixl_expr(ws(alt((op_plus, op_minus))), fw_expr12, fw_expr12),
            fw_expr12,
        )),
    )(input)
}

fn fw_expr10<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "expr10",
        alt((
            infixl_expr(
                ws(alt((op_lt, op_gt, op_lte, op_gte))),
                fw_expr11,
                fw_expr11,
            ),
            fw_expr11,
        )),
    )(input)
}

fn fw_expr9<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context(
        "expr9",
        alt((
            prefix_expr(ws(op_minus), fw_expr10),
            fw_expr10,
        )),
    )(input)
}

pub fn fw_expr<'a, E>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
{
    context("expr", fw_expr9)(input)
}

#[cfg(test)]
mod tests {
    use crate::*;

    macro_rules! assert_parse {
        ($input:expr, $expected:expr) => {
            let result = fw_expr::<nom::error::VerboseError<&str>>($input);
            if let Err(nom::Err::Error(ref e)) = result {
                println!("Parse Error: \n{}", nom::error::convert_error($input, e.clone()));
            }

            std::assert_matches::assert_matches!(result, Ok((_, ref e)) if e == &$expected);
        };
    }

    #[test]
    fn test_parse_identifier() {
        assert_parse!("foo", Expr::ident("foo"));
        assert_parse!("foo_bar", Expr::ident("foo_bar"));
        assert_parse!("foo_bar_", Expr::ident("foo_bar_"));
        assert_parse!("foo_bar_12", Expr::ident("foo_bar_12"));
        assert_parse!("foobar", Expr::ident("foobar"));
        assert_parse!("foobar12", Expr::ident("foobar12"));
        assert_parse!("_foobar12", Expr::ident("_foobar12"));
    }

    #[test]
    fn test_parse_literal() {
        assert_parse!("true", Expr::literal(true));
        assert_parse!("false", Expr::literal(false));
        assert_parse!("123", Expr::literal(123));
    }

    #[test]
    fn test_parse_unop() {
        assert_parse!(" -foo", Expr::unop(OpKind::Sub, Expr::ident("foo")));
        assert_parse!("(-42) ", Expr::unop(OpKind::Sub, Expr::literal(42)));
        assert_parse!("(- 42)", Expr::unop(OpKind::Sub, Expr::literal(42)));
        assert_parse!("(-x^2)", Expr::unop(OpKind::Sub, 
            Expr::binop(OpKind::Pow, Expr::ident("x"), Expr::literal(2))
        ));
    }

    #[test]
    fn test_parse_binop() {
        assert_parse!(
            "foo + bar + baz",
            Expr::binop(
                OpKind::Add,
                Expr::binop(OpKind::Add, Expr::ident("foo"), Expr::ident("bar")),
                Expr::ident("baz")
            )
        );
        assert_parse!(
            "foo - bar - baz",
            Expr::binop(
                OpKind::Sub,
                Expr::binop(OpKind::Sub, Expr::ident("foo"), Expr::ident("bar")),
                Expr::ident("baz")
            )
        );
        assert_parse!(
            "foo * bar * baz",
            Expr::binop(
                OpKind::Mul,
                Expr::binop(OpKind::Mul, Expr::ident("foo"), Expr::ident("bar")),
                Expr::ident("baz")
            )
        );
        assert_parse!(
            "foo / bar / baz",
            Expr::binop(
                OpKind::Div,
                Expr::binop(OpKind::Div, Expr::ident("foo"), Expr::ident("bar")),
                Expr::ident("baz")
            )
        );
        assert_parse!(
            "foo / (foo + bar)",
            Expr::binop(
                OpKind::Div,
                Expr::ident("foo"),
                Expr::binop(OpKind::Add, Expr::ident("foo"), Expr::ident("bar"))
            )
        );
        assert_parse!(
            "(foo + bar) / foo",
            Expr::binop(
                OpKind::Div,
                Expr::binop(OpKind::Add, Expr::ident("foo"), Expr::ident("bar")),
                Expr::ident("foo")
            )
        );
    }

    #[test]
    fn test_operator_precedence() {
        assert_parse!(
            "foo + bar * baz",
            Expr::binop(
                OpKind::Add,
                Expr::ident("foo"),
                Expr::binop(OpKind::Mul, Expr::ident("bar"), Expr::ident("baz"))
            )
        );
        assert_parse!(
            "foo * bar + baz",
            Expr::binop(
                OpKind::Add,
                Expr::binop(OpKind::Mul, Expr::ident("foo"), Expr::ident("bar")),
                Expr::ident("baz")
            )
        );
        assert_parse!(
            "foo + bar / baz",
            Expr::binop(
                OpKind::Add,
                Expr::ident("foo"),
                Expr::binop(OpKind::Div, Expr::ident("bar"), Expr::ident("baz"))
            )
        );
        assert_parse!(
            "foo < bar + baz",
            Expr::binop(
                OpKind::Lt,
                Expr::ident("foo"),
                Expr::binop(OpKind::Add, Expr::ident("bar"), Expr::ident("baz"))
            )
        );
        assert_parse!(
            "baz + foo < bar - foo",
            Expr::binop(
                OpKind::Lt,
                Expr::binop(OpKind::Add, Expr::ident("baz"), Expr::ident("foo")),
                Expr::binop(OpKind::Sub, Expr::ident("bar"), Expr::ident("foo")),
            )
        );
        assert_parse!(
            "foo + foo^bar^baz + baz",
            Expr::binop(
                OpKind::Add,
                Expr::binop(
                    OpKind::Add,
                    Expr::ident("foo"),
                    Expr::binop(
                        OpKind::Pow,
                        Expr::ident("foo"),
                        Expr::binop(OpKind::Pow, 
                            Expr::ident("bar"), 
                            Expr::ident("baz")
                        )
                    ),
                ),
                Expr::ident("baz")
            )
        );
    }
}

fn main() {
    println!(
        "{:#?}",
        fw_expr::<nom::error::VerboseError<&str>>("(-x^2)")
            .unwrap()
            .1
    );
}
