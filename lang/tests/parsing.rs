use lang::parsing::{Declaration, Expr, Statement, ParamsList, Bindings, Ident};

macro_rules! assert_parse_expr {
    ($input:expr, $expected:expr) => {
        match lang::parsing::parse_expr($input) {
            Ok(o) => assert_eq!(o, $expected),
            Err(e) => panic!("Failed to parse expression:\n{}", e)
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
    assert_parse_expr!("foo", Expr::ident_name("foo"));
    assert_parse_expr!("foo_bar", Expr::ident_name("foo_bar"));
    assert_parse_expr!("foo_bar_", Expr::ident_name("foo_bar_"));
    assert_parse_expr!("foo_bar_12", Expr::ident_name("foo_bar_12"));
    assert_parse_expr!("foobar", Expr::ident_name("foobar"));
    assert_parse_expr!("foobar12", Expr::ident_name("foobar12"));
    assert_parse_expr!("_foobar12", Expr::ident_name("_foobar12"));
}

#[test]
fn test_parse_literal() {
    assert_parse_expr!("()", Expr::Unit);
    assert_parse_expr!("true", true.into());
    assert_parse_expr!("false", false.into());
    assert_parse_expr!("123", Expr::Int32(123));
    assert_parse_expr!("123.456", Expr::Float32(123.456));
    assert_parse_expr!("\"foo\"", Expr::String("foo".into()));
}

#[test]
fn test_function_application() {
    assert_parse_expr!(
        "foo bar",
        Expr::fnapp(Expr::ident_name("foo"), Expr::ident_name("bar"))
    );
    assert_parse_expr!(
        "foo bar baz",
        Expr::fnapp(
            Expr::fnapp(Expr::ident_name("foo"), Expr::ident_name("bar")),
            Expr::ident_name("baz")
        )
    );
    assert_parse_expr!(
        "foo (bar baz)",
        Expr::fnapp(
            Expr::ident_name("foo"),
            Expr::fnapp(Expr::ident_name("bar"), Expr::ident_name("baz"))
        )
    );
}

#[test]
fn test_operator_function_application() {
    assert_parse_expr!(
        "foo + bar",
        Expr::fnapp(
            Expr::fnapp(Expr::ident_name("+"), Expr::ident_name("foo")),
            Expr::ident_name("bar")
        )
    );
}

#[test]
fn test_if_expr() {
    assert_parse_expr!(
        "if true then 1 else 2",
        Expr::ife(true.into(), 1.into(), 2.into())
    );
    assert_parse_expr!(
        "if false then 1 else (if false then 2 else 3)",
        Expr::ife(
            false.into(),
            1.into(),
            Expr::ife(false.into(), 2.into(), 3.into())
        )
    );
}

#[test]
fn test_let_block() {
    assert_parse_statement!(
        "let x = 2 then x",
        Statement::block(
            Bindings::new(vec![
                Declaration::constant(Ident::name("x"), 2.into()),
            ]),
            Expr::ident_name("x")
        )
    );
    assert_parse_statement!(
        "let x = 3\n\ty = 4 then x + y",
        Statement::block(
            Bindings::new(vec![
                Declaration::constant(Ident::name("x"), 3.into()),
                Declaration::constant(Ident::name("y"), 4.into()),
            ]),
            Expr::fnapp2(Expr::ident_name("+"), Expr::ident_name("x"), Expr::ident_name("y"))
        )
    );
    assert_parse_statement!(
        "let x = 5\n\ty = 6\n\tthen\n\tx + y",
        Statement::block(
            Bindings::new(vec![
                Declaration::constant(Ident::name("x"), 5.into()),
                Declaration::constant(Ident::name("y"), 6.into()),
            ]),
            Expr::fnapp2(Expr::ident_name("+"), Expr::ident_name("x"), Expr::ident_name("y"))
        )
    );
    assert_parse_statement!(
        "let add5 x = 5 + x\ntimes2 x = 2 * x\n\tthen\n\tadd5 >> times2",
        Statement::block(
            Bindings::new(vec![
                Declaration::function(
                    Ident::name("add5"),
                    ParamsList::new(vec![Ident::name("x")]),
                    Expr::fnapp2(Expr::ident_name("+"), 5.into(), Expr::ident_name("x")).into()
                ),
                Declaration::function(
                    Ident::name("times2"),
                    ParamsList::new(vec![Ident::name("x")]),
                    Expr::fnapp2(Expr::ident_name("*"), 2.into(), Expr::ident_name("x")).into()
                ),
            ]),
            Expr::fnapp2(Expr::ident_name(">>"), Expr::ident_name("add5"), Expr::ident_name("times2"))
        )
    );
}

#[test]
fn test_func_decl() {
    assert_parse_statement!(
        "add1 x y = x + y",
        Statement::declaration(
            Declaration::Function {
                name: Ident::name("add1"),
                params: ParamsList::new(vec![Ident::name("x"), Ident::name("y")]),
                body: Box::new(Expr::fnapp2(Expr::ident_op("+"), Expr::ident_name("x"), Expr::ident_name("y")).into()),
            }
        )
    );

    assert_parse_statement!(
        "add2 x y = \nlet k = x \n g = y \n then k + g",
        Statement::declaration(
            Declaration::Function {
                name: Ident::name("add2"),
                params: ParamsList::new(vec![Ident::name("x"), Ident::name("y")]),
                body: Box::new(
                    Statement::block(
                        Bindings::new(vec![
                            Declaration::constant(Ident::name("k"), Expr::ident_name("x")),
                            Declaration::constant(Ident::name("g"), Expr::ident_name("y")),
                        ]),
                        Expr::fnapp2(Expr::ident_op("+"), Expr::ident_name("k"), Expr::ident_name("g"))
                    )
                ),
            }
        )
    );
}