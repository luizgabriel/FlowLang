#![feature(assert_matches)]

use lang::ast::Value;

macro_rules! assert_evals {
    ($($input:expr => $expected:expr),*) => {
        let pairs: Vec<(&'static str, lang::ast::Value)> = vec![$(($input, $expected)),*];
        pairs
            .iter()
            .map(|(input, expected)| (lang::parsing::parse(input).unwrap(), expected))
            .fold(lang::evaluation::Environment::new_with_std(), |acc, (expr, expected)| -> lang::evaluation::Environment {
                let (value, next_env) = lang::evaluation::eval(&expr, acc).unwrap();
                assert_eq!(&value, expected);
                next_env
            });
    };

    ($input:expr => $expected:expr) => {
        assert_evals!(Environment::new_with_std(), $input => $expected);
    };
}

#[test]
fn test_const_definition() {
    assert_evals! {
        "k = 5" => Value::Unit(),
        "k" => Value::Int32(5)
    }
}

#[test]
fn test_bultins() {
    assert_evals! {
        "5 * 2" => Value::Int32(10),
        "10 + 50" => Value::Int32(60),
        "10 - 50" => Value::Int32(-40),
        "10 / 2" => Value::Int32(5),
        "5.0 / 2.0" => Value::Float32(2.5)
    }
}

#[test]
fn test_lambda() {
    assert_evals! {
        "f = (x -> x + 1)" => Value::Unit(),
        "f 5" => Value::Int32(6)
    }
}

#[test]
fn test_function_declaration() {
    assert_evals! {
        "square x = x * x" => Value::Unit(),
        "hypotenuse x y = square x + square y" => Value::Unit(),
        "hypotenuse 3 4" => Value::Int32(25)
    }
}

#[test]
fn test_pipe_operator() {
    assert_evals! {
        "f = (+) 1" => Value::Unit(),
        "g = (*) 2" => Value::Unit(),
        "5 |> f |> g" => Value::Int32(12)
    }
}
