macro_rules! assert_eval {
    ($($input:expr => $expected:expr),*) => {
        use lang::evaluation::env::ValueEnvironment;
        use lang::evaluation::data::Value;
        use lang::evaluation::Evaluator;
        use lang::parsing::parse_program;

        let pairs: Vec<(&'static str, Value)> = vec![$(($input, $expected)),*];
        pairs
            .iter()
            .map(|(input, expected)| (parse_program(input).unwrap(), expected))
            .fold((ValueEnvironment::default().import_std(), 1), |(acc, line), (program, expected)| -> (ValueEnvironment, usize) {
                match program.eval(acc) {
                    Ok((result, env)) => {
                        assert_eq!(&result, expected, "Asserting {}: Expected {} at line {}", result, expected, line);
                        (env, line + 1)
                    },

                    Err(err) => {
                        panic!("Error: {} at line {}", err, line)
                    },
                }
            });
    };
}

#[test]
fn test_const_definition() {
    assert_eval! {
        "k = 5" => Value::Unit,
        "k" => Value::Int32(5)
    }
}

#[test]
fn test_builtin() {
    assert_eval! {
        "5 * 2" => Value::Int32(10),
        "10 + 50" => Value::Int32(60),
        "10 - 50" => Value::Int32(-40),
        "10 / 2" => Value::Int32(5),
        "compare = equalUpTo 0.0001" => Value::Unit,
        "compare (5.0 / 2.0) 2.5" => Value::Bool(true),
        "compare (1.1 + 2.2 - 5.0) -1.7" => Value::Bool(true)
    }
}

#[test]
fn test_lambda() {
    assert_eval! {
        "f = (x -> x + 1)" => Value::Unit,
        "f 5" => Value::Int32(6)
    }
}

#[test]
fn test_function_declaration() {
    assert_eval! {
        "square x = x * x" => Value::Unit,
        "hypotenuse x y = sqrt (square x + square y)" => Value::Unit,
        "hypotenuse 3 4" => Value::Float32(5.0)
    }
}

#[test]
fn test_pipe_operator() {
    assert_eval! {
        "f = (+) 1" => Value::Unit,
        "g = (*) 2" => Value::Unit,
        "5 |> f |> g" => Value::Int32(12)
    }
}

#[test]
fn test_string_concatenation() {
    assert_eval! {
        "s = \"foo\"" => Value::Unit,
        "t = \"bar\"" => Value::Unit,
        "s ++ t" => Value::String("foobar".to_string())
    }
}
