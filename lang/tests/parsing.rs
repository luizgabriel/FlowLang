use lang::parsing::{
    ast::{Expr, Module},
    parse_module,
};

macro_rules! assert_parse {
    ($input:expr, $expected:expr) => {
        let expr = lang::parsing::parse_expr($input);
        assert_eq!(expr, Ok($expected));
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
    assert_parse!("()", Expr::Unit);
    assert_parse!("true", Expr::Bool(true));
    assert_parse!("false", Expr::Bool(false));
    assert_parse!("123", Expr::Int32(123));
    assert_parse!("123.456", Expr::Float32(123.456));
    assert_parse!("\"foo\"", Expr::String("foo".into()));
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
        Expr::opapp(Expr::ident("+"), Expr::ident("foo"), Expr::ident("bar"))
    );
    assert_parse!(
        "(foo |> bar) |> baz",
        Expr::opapp(
            Expr::ident("|>"),
            Expr::opapp(Expr::ident("|>"), Expr::ident("foo"), Expr::ident("bar")),
            Expr::ident("baz"),
        )
    );
    assert_parse!(
        "foo |> (bar |> baz)",
        Expr::opapp(
            Expr::ident("|>"),
            Expr::ident("foo"),
            Expr::opapp(Expr::ident("|>"), Expr::ident("bar"), Expr::ident("baz")),
        )
    );
}

#[test]
fn test_if_expr() {
    assert_parse!(
        "if true then 1 else 2",
        Expr::ife(Expr::Bool(true), Expr::Int32(1), Expr::Int32(2))
    );
    assert_parse!(
        "if false then 1 else (if false then 2 else 3)",
        Expr::ife(
            Expr::Bool(false),
            Expr::Int32(1),
            Expr::ife(Expr::Bool(false), Expr::Int32(2), Expr::Int32(3))
        )
    );
}

macro_rules! asserts_parse_module {
    ($input:expr, $excepted:expr) => {
        match parse_module($input) {
            Ok(expr) => assert_eq!(expr, Module::new($excepted)),
            Err(err) => panic!("Error: \n{}", err),
        }
    };
}

#[test]
fn test_module() {
    asserts_parse_module!(
        "x = 2\ny = 3\n",
        vec![
            Expr::constdef("x".into(), Expr::Int32(2)),
            Expr::constdef("y".into(), Expr::Int32(3))
        ]
    );
    asserts_parse_module!(
        "\n\nx = 5\n\n\n\ty = 6\t\n",
        vec![
            Expr::constdef("x".into(), Expr::Int32(5)),
            Expr::constdef("y".into(), Expr::Int32(6))
        ]
    );
}
