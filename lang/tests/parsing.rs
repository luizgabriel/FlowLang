#![feature(assert_matches)]

use lang::ast::{Expr, OpKind};

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
fn test_parse_unop() {
    assert_parse!(" -foo", Expr::unop(OpKind::Sub, Expr::ident("foo")));
    assert_parse!("(-42) ", Expr::unop(OpKind::Sub, Expr::literal(42)));
    assert_parse!("(- 42)", Expr::unop(OpKind::Sub, Expr::literal(42)));
    assert_parse!(
        "(-x^2)",
        Expr::unop(
            OpKind::Sub,
            Expr::binop(OpKind::Pow, Expr::ident("x"), Expr::literal(2))
        )
    );

    assert_parse!(
        "x + -2*y",
        Expr::binop(
            OpKind::Add,
            Expr::ident("x"),
            Expr::binop(
                OpKind::Mul,
                Expr::unop(OpKind::Sub, Expr::literal(2)),
                Expr::ident("y")
            )
        )
    );
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
                    Expr::binop(OpKind::Pow, Expr::ident("bar"), Expr::ident("baz"))
                ),
            ),
            Expr::ident("baz")
        )
    );
}
