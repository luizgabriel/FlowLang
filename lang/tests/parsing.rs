use std::borrow::Cow;

use im::vector;
use lang::parsing::{data::IdentConstructor, Expr, Ident, ParamsList, Statement};

macro_rules! assert_parse_expr {
    ($input:expr, $expected:expr) => {
        match lang::parsing::parse_expr($input) {
            Ok(o) => assert_eq!(o, $expected),
            Err(e) => panic!("Failed to parse expression:\n{}", e),
        }
    };
}

macro_rules! assert_parse_statement {
    ($input:expr, $expected:expr) => {
        match lang::parsing::parse_statement($input) {
            Ok(o) => assert_eq!(o, $expected),
            Err(e) => panic!("Failed to parse statement:\n{}", e),
        }
    };
}

#[test]
fn test_parse_identifier() {
    assert_parse_expr!("foo", Ident::name("foo").into());
    assert_parse_expr!("foo_bar", Ident::name("foo_bar").into());
    assert_parse_expr!("foo_bar_", Ident::name("foo_bar_").into());
    assert_parse_expr!("foo_bar_12", Ident::name("foo_bar_12").into());
    assert_parse_expr!("foobar", Ident::name("foobar").into());
    assert_parse_expr!("foobar12", Ident::name("foobar12").into());
    assert_parse_expr!("_foobar12", Ident::name("_foobar12").into());
}

#[test]
fn test_parse_literal() {
    assert_parse_expr!("()", Expr::Unit);
    assert_parse_expr!("true", Expr::Bool(true));
    assert_parse_expr!("false", Expr::Bool(false));
    assert_parse_expr!("123", Expr::Int32(123));
    assert_parse_expr!("123.456", Expr::Float32(123.456));
    assert_parse_expr!("\"foo\"", Expr::String("foo".into()));
}

#[test]
fn test_function_application() {
    assert_parse_expr!(
        "foo bar",
        Expr::fnapp(Ident::name("foo").into(), Ident::name("bar").into())
    );
    assert_parse_expr!(
        "foo bar baz",
        Expr::fnapp(
            Expr::fnapp(Ident::name("foo").into(), Ident::name("bar").into()),
            Ident::name("baz").into()
        )
    );
    assert_parse_expr!(
        "foo (bar baz)",
        Expr::fnapp(
            Ident::name("foo").into(),
            Expr::fnapp(Ident::name("bar").into(), Ident::name("baz").into())
        )
    );
}

#[test]
fn test_operator_function_application() {
    assert_parse_expr!(
        "foo + bar",
        Expr::fnapp(
            Expr::fnapp(Ident::op("+").into(), Ident::name("foo").into()),
            Ident::name("bar").into()
        )
    );
}

#[test]
fn test_if_expr() {
    assert_parse_expr!(
        "if true then 1 else 2",
        Expr::ife(Expr::Bool(true), Expr::Int32(1), Expr::Int32(2))
    );
    assert_parse_expr!(
        "if false then 1 else (if false then 2 else 3)",
        Expr::ife(
            Expr::Bool(false),
            Expr::Int32(1),
            Expr::ife(Expr::Bool(false), Expr::Int32(2), Expr::Int32(3))
        )
    );
}

#[test]
fn test_let_block() {
    assert_parse_statement!(
        "{ x = 2; x }",
        Statement::Block(vector![
            Statement::constant(Ident::name("x"), Expr::Int32(2)),
            Ident::name("x").into(),
        ])
    );
    assert_parse_statement!(
        "{ x = 3;\n\ty = 4; x + y }",
        Statement::Block(vector![
            Statement::constant(Ident::name("x"), Expr::Int32(3)),
            Statement::constant(Ident::name("y"), Expr::Int32(4)),
            Expr::fnapp2(
                Ident::op("+").into(),
                Ident::name("x").into(),
                Ident::name("y").into()
            )
            .into(),
        ])
    );
    assert_parse_statement!(
        "{ x = 5;\n\ty = 6; \n\tx + y }",
        Statement::Block(vector![
            Statement::constant(Ident::name("x"), Expr::Int32(5)),
            Statement::constant(Ident::name("y"), Expr::Int32(6)),
            Expr::fnapp2(
                Ident::op("+").into(),
                Ident::name("x").into(),
                Ident::name("y").into()
            )
            .into(),
        ])
    );
    assert_parse_statement!(
        "{ add5 x = 5 + x; \ntimes2 x = 2 * x;\n\tadd5 >> times2 }",
        Statement::Block(vector![
            Statement::function(
                Ident::name("add5"),
                ParamsList::new(vector![Ident::name("x")]),
                Expr::fnapp2(
                    Ident::op("+").into(),
                    Expr::Int32(5),
                    Ident::name("x").into()
                )
                .into()
            ),
            Statement::function(
                Ident::name("times2"),
                ParamsList::new(vector![Ident::name("x")]),
                Expr::fnapp2(
                    Ident::op("*").into(),
                    Expr::Int32(2),
                    Ident::name("x").into()
                )
                .into()
            ),
            Expr::fnapp2(
                Ident::op(">>").into(),
                Ident::name("add5").into(),
                Ident::name("times2").into(),
            )
            .into(),
        ])
    );
}

#[test]
fn test_func_decl() {
    assert_parse_statement!(
        "add1 x y = x + y",
        Statement::function(
            Ident::name("add1"),
            ParamsList::new(vector![Ident::name("x"), Ident::name("y")]),
            Expr::fnapp2(
                Ident::op("+").into(),
                Ident::name("x").into(),
                Ident::name("y").into()
            )
            .into()
        )
    );

    assert_parse_statement!(
        "add2 x y = \n{ k = x; g = y; k + g }",
        Statement::function(
            Ident::name("add2"),
            ParamsList::new(vector![Ident::name("x"), Ident::name("y")]),
            Statement::Block(vector![
                Statement::constant(Ident::name("k"), Ident::name("x").into()),
                Statement::constant(Ident::name("g"), Ident::name("y").into()),
                Expr::fnapp2(
                    Ident::op("+").into(),
                    Ident::name("k").into(),
                    Ident::name("g").into()
                )
                .into()
            ]),
        )
    );
}
