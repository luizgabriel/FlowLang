use crate::evaluation::{Environment, Evaluator};
use crate::evaluation::{EvalError, Value, ValueEnvironment};
use crate::parsing::data::IdentConstructor;
use crate::parsing::Ident;

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltInFunc {
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
    Add,
    Sub,
    Mul,
    Div,
    Abs,
    Sqrt,
    Concat,
    PowF,
    PowI,
}

fn eval_comparison(
    name: &BuiltInFunc,
    env: &ValueEnvironment,
) -> Result<(Value, ValueEnvironment), EvalError> {
    let x = env.get(&Ident::name("lhs")).unwrap();
    let y = env.get(&Ident::name("rhs")).unwrap();

    match (name, x, y) {
        (BuiltInFunc::Eq, Value::Int32(x), Value::Int32(y)) => env.pure((x == y).into()),
        (BuiltInFunc::Eq, Value::Float32(x), Value::Float32(y)) => env.pure((x == y).into()),
        (BuiltInFunc::Eq, Value::Bool(x), Value::Bool(y)) => env.pure((x == y).into()),
        (BuiltInFunc::Gt, Value::Int32(x), Value::Int32(y)) => env.pure((x > y).into()),
        (BuiltInFunc::Gt, Value::Float32(x), Value::Float32(y)) => env.pure((x > y).into()),
        (BuiltInFunc::Lt, Value::Int32(x), Value::Int32(y)) => env.pure((x < y).into()),
        (BuiltInFunc::Lt, Value::Float32(x), Value::Float32(y)) => env.pure((x < y).into()),
        (BuiltInFunc::Gte, Value::Int32(x), Value::Int32(y)) => env.pure((x >= y).into()),
        (BuiltInFunc::Gte, Value::Float32(x), Value::Float32(y)) => env.pure((x >= y).into()),
        (BuiltInFunc::Lte, Value::Int32(x), Value::Int32(y)) => env.pure((x <= y).into()),
        (BuiltInFunc::Lte, Value::Float32(x), Value::Float32(y)) => env.pure((x <= y).into()),
        (_, Value::Int32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: "Int32",
        }),
        (_, Value::Float32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: "Float32",
        }),
        (_, x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Int32 or Float32",
        }),
    }
}

fn eval_concat(env: &ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
    let x: String = env.get(&Ident::name("lhs")).unwrap().clone().try_into()?;
    let y: String = env.get(&Ident::name("rhs")).unwrap().clone().try_into()?;

    env.pure(format!("{x}{y}").into())
}

fn eval_math(
    name: &BuiltInFunc,
    env: &ValueEnvironment,
) -> Result<(Value, ValueEnvironment), EvalError> {
    let x = env.get(&Ident::name("lhs")).unwrap();
    let y = env.get(&Ident::name("rhs")).unwrap();

    match (name, x, y) {
        (BuiltInFunc::Add, Value::Int32(x), Value::Int32(y)) => env.pure((x + y).into()),
        (BuiltInFunc::Add, Value::Float32(x), Value::Float32(y)) => env.pure((x + y).into()),
        (BuiltInFunc::Sub, Value::Int32(x), Value::Int32(y)) => env.pure((x - y).into()),
        (BuiltInFunc::Sub, Value::Float32(x), Value::Float32(y)) => env.pure((x - y).into()),
        (BuiltInFunc::Mul, Value::Int32(x), Value::Int32(y)) => env.pure((x * y).into()),
        (BuiltInFunc::Mul, Value::Float32(x), Value::Float32(y)) => env.pure((x * y).into()),
        (BuiltInFunc::Div, Value::Int32(x), Value::Int32(y)) => env.pure((x / y).into()),
        (BuiltInFunc::Div, Value::Float32(x), Value::Float32(y)) => env.pure((x / y).into()),

        (_, Value::Int32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: "Int32",
        }),
        (_, Value::Float32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: "Float32",
        }),
        (_, x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Int32 or Float32",
        }),
    }
}

fn eval_abs(env: &ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
    let x = env.get(&Ident::name("x")).unwrap();

    match x {
        Value::Int32(x) => env.pure(x.abs().into()),
        Value::Float32(x) => env.pure(x.abs().into()),
        x => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Int32 or Float32",
        }),
    }
}

fn eval_sqrt(env: &ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
    let x: &Value = env.get(&Ident::name("x")).unwrap();

    match x {
        Value::Int32(x) => env.pure((*x as f32).sqrt().into()),
        Value::Float32(x) => env.pure(x.sqrt().into()),
        x => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Int32 or Float32",
        }),
    }
}

fn eval_powf(env: &ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
    let x = env.get(&Ident::name("lhs")).unwrap();
    let y = env.get(&Ident::name("rhs")).unwrap();

    match (x, y) {
        (Value::Float32(x), Value::Float32(y)) => env.pure(x.powf(*y).into()),
        (Value::Float32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: "Float32",
        }),
        (x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Float32",
        }),
    }
}

fn eval_powi(env: &ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
    let x = env.get(&Ident::name("lhs")).unwrap();
    let y = env.get(&Ident::name("rhs")).unwrap();

    match (x, y) {
        (Value::Float32(x), Value::Int32(y)) => env.pure((x.powi(*y)).into()),
        (Value::Float32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: "Int32",
        }),
        (x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Float32",
        }),
    }
}

impl Evaluator for BuiltInFunc {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
        match self {
            BuiltInFunc::Concat => eval_concat(&env),
            BuiltInFunc::Sqrt => eval_sqrt(&env),
            BuiltInFunc::PowF => eval_powf(&env),
            BuiltInFunc::PowI => eval_powi(&env),
            BuiltInFunc::Eq
            | BuiltInFunc::Gt
            | BuiltInFunc::Lt
            | BuiltInFunc::Gte
            | BuiltInFunc::Lte => eval_comparison(self, &env),
            BuiltInFunc::Abs => eval_abs(&env),
            BuiltInFunc::Add | BuiltInFunc::Sub | BuiltInFunc::Mul | BuiltInFunc::Div => {
                eval_math(self, &env)
            }
        }
    }
}
