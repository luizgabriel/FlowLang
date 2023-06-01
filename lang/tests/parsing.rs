use lang::parsing::{Expr, Ident, ParamsList, Statement};

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
    assert_parse_expr!("foo", Expr::name("foo"));
    assert_parse_expr!("foo_bar", Expr::name("foo_bar"));
    assert_parse_expr!("foo_bar_", Expr::name("foo_bar_"));
    assert_parse_expr!("foo_bar_12", Expr::name("foo_bar_12"));
    assert_parse_expr!("foobar", Expr::name("foobar"));
    assert_parse_expr!("foobar12", Expr::name("foobar12"));
    assert_parse_expr!("_foobar12", Expr::name("_foobar12"));
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
    assert_parse_expr!("foo bar", Expr::fnapp(Expr::name("foo"), Expr::name("bar")));
    assert_parse_expr!(
        "foo bar baz",
        Expr::fnapp(
            Expr::fnapp(Expr::name("foo"), Expr::name("bar")),
            Expr::name("baz")
        )
    );
    assert_parse_expr!(
        "foo (bar baz)",
        Expr::fnapp(
            Expr::name("foo"),
            Expr::fnapp(Expr::name("bar"), Expr::name("baz"))
        )
    );
}

#[test]
fn test_operator_function_application() {
    assert_parse_expr!(
        "foo + bar",
        Expr::fnapp(
            Expr::fnapp(Expr::operator("+"), Expr::name("foo")),
            Expr::name("bar")
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
        "let x = 2 then x",
        Statement::block(
            vec![
                Statement::constant(
                    Ident::name("x"),
                    Expr::Int32(2)
                ),
            ],
            Expr::name("x")
        )
    );
    assert_parse_statement!(
        "let x = 3\n\ty = 4 then x + y",
        Statement::block(
            vec![
                Statement::constant(Ident::name("x"), Expr::Int32(3)),
                Statement::constant(Ident::name("y"), Expr::Int32(4)),
            ],
            Expr::fnapp2(Expr::operator("+"), Expr::name("x"), Expr::name("y"))
        )
    );
    assert_parse_statement!(
        "let x = 5\n\ty = 6\n\tthen\n\tx + y",
        Statement::block(
            vec![
                Statement::constant(Ident::name("x"), Expr::Int32(5)),
                Statement::constant(Ident::name("y"), Expr::Int32(6)),
            ],
            Expr::fnapp2(Expr::operator("+"), Expr::name("x"), Expr::name("y"))
        )
    );
    assert_parse_statement!(
        "let add5 x = 5 + x\ntimes2 x = 2 * x\n\tthen\n\tadd5 >> times2",
        Statement::block(
            vec![
                Statement::function(
                    Ident::name("add5"),
                    ParamsList::new(vec![Ident::name("x")]),
                    Expr::fnapp2(Expr::operator("+"), Expr::Int32(5), Expr::name("x")).into()
                ),
                Statement::function(
                    Ident::name("times2"),
                    ParamsList::new(vec![Ident::name("x")]),
                    Expr::fnapp2(Expr::operator("*"), Expr::Int32(2), Expr::name("x")).into()
                ),
            ],
            Expr::fnapp2(
                Expr::operator(">>"),
                Expr::name("add5"),
                Expr::name("times2")
            )
        )
    );
}

#[test]
fn test_func_decl() {
    assert_parse_statement!(
        "add1 x y = x + y",
        Statement::function(
            Ident::name("add1"),
            ParamsList::new(vec![Ident::name("x"), Ident::name("y")]),
            Expr::fnapp2(Expr::operator("+"), Expr::name("x"), Expr::name("y")).into()
        )
    );

    assert_parse_statement!(
        "add2 x y = \nlet k = x \n g = y \n then k + g",
        Statement::function(
            Ident::name("add2"),
            ParamsList::new(vec![Ident::name("x"), Ident::name("y")]),
            Statement::block(
                vec![
                    Statement::constant(Ident::name("k"), Expr::name("x")),
                    Statement::constant(Ident::name("g"), Expr::name("y")),
                ],
                Expr::fnapp2(Expr::operator("+"), Expr::name("k"), Expr::name("g"))
            ),
        )
    );
}
