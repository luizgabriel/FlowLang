#![feature(assert_matches)]

use lang::ast::Expr;

macro_rules! assert_parse {
        ($input:expr, $expected:expr) => {
            let result = lang::parsing::parse($input);
            if let Err(ref e) = result {
                println!("Parse Error: \n{}", e);
            }

            std::assert_matches::assert_matches!(result, Ok(expr) if expr == $expected);
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
fn test_function_application() {
    assert_parse!(
        "foo bar",
        Expr::fnapp(Expr::ident("foo"), Expr::ident("bar"))
    );
    assert_parse!(
        "foo bar baz",
        Expr::fnapp(
            Expr::fnapp(Expr::ident("foo"), Expr::ident("bar")),
            Expr::ident("baz")
        )
    );
    assert_parse!(
        "foo (bar baz)",
        Expr::fnapp(
            Expr::ident("foo"),
            Expr::fnapp(Expr::ident("bar"), Expr::ident("baz"))
        )
    );
}

#[test]
fn test_operator_function_application() {
    assert_parse!(
        "foo + bar",
        Expr::fnapp(
            Expr::fnapp(Expr::operator("+"), Expr::ident("foo")),
            Expr::ident("bar")
        )
    );
}
