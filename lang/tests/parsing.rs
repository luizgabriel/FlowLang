use lang::parsing::data::Expr;

macro_rules! assert_parse {
    ($input:expr, $expected:expr) => {
        let expr = lang::parsing::parse_expr($input).unwrap();
        assert_eq!(expr, $expected);
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
        Expr::fnapp(
            Expr::fnapp(Expr::ident("+"), Expr::ident("foo")),
            Expr::ident("bar")
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
