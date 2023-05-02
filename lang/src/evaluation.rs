use crate::{
    ast::{BuiltInFunc, Expr, Value},
    env::Environment,
    error::EvalError,
};

type EvalResult = Result<(Value, Environment), EvalError>;

pub fn eval(expr: Expr, env: Environment) -> EvalResult {
    match expr {
        Expr::Unit => Ok((Value::Unit(), env)),
        Expr::Bool(value) => Ok((Value::Bool(value), env)),
        Expr::Int32(value) => Ok((Value::Int32(value), env)),
        Expr::Float32(value) => Ok((Value::Float32(value), env)),
        Expr::String(value) => Ok((Value::String(value), env)),

        Expr::Identifier(ident) => {
            let value = env
                .get(&ident)
                .ok_or_else(|| EvalError::UnboundIdentifier(ident))?;

            Ok((value.clone(), env))
        }

        Expr::Lambda { params, body } => {
            let function = Value::Function {
                params,
                body,
                scope: env.clone(),
            };

            Ok((function, env))
        }

        Expr::ConstantDefinition { name, value } => {
            let (value, env) = eval(*value, env)?;
            Ok((Value::Unit(), env.set(name, value)))
        }

        Expr::FunctionApplication(lhs, rhs) => {
            let (lhs, env) = eval(*lhs, env)?;
            let (rhs, env) = eval(*rhs, env)?;

            match lhs {
                Value::BuiltInFunction {
                    name,
                    params,
                    scope,
                } => {
                    let (param, rest) = params.split_first().unwrap();
                    let scope = scope.clone().set(param.clone(), rhs);

                    if rest.is_empty() {
                        let result = eval_builtin_function(&name, &scope)?;
                        return Ok((result, env));
                    }

                    Ok((
                        Value::BuiltInFunction {
                            name,
                            params: rest.iter().cloned().collect(),
                            scope,
                        },
                        env,
                    ))
                }

                Value::Function {
                    params,
                    body,
                    scope,
                } => {
                    let (param, rest) = params.split_first().unwrap();
                    let scope = scope.clone().set(param.clone(), rhs);

                    if rest.is_empty() {
                        let (result, _) = eval(*body, scope)?;
                        return Ok((result, env));
                    }

                    Ok((
                        Value::Function {
                            params: rest.iter().cloned().collect(),
                            body,
                            scope,
                        },
                        env,
                    ))
                }

                _ => Err(EvalError::NotAFunction(lhs)),
            }
        }

        Expr::FunctionDefinition { name, params, body } => {
            let function = Value::Function {
                params,
                body,
                scope: env.clone(),
            };

            Ok((Value::Unit(), env.set(name, function)))
        }

        Expr::If {
            condition,
            then,
            otherwise,
        } => {
            let (condition, env) = eval(*condition, env)?;

            if condition.try_into()? {
                eval(*then, env)
            } else {
                eval(*otherwise, env)
            }
        }
    }
}

fn eval_abs(env: &Environment) -> Result<Value, EvalError> {
    let x = env.get(&"x".into()).unwrap();

    return match x {
        Value::Int32(x) => Ok(x.abs().into()),
        Value::Float32(x) => Ok(x.abs().into()),
        _ => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: "Int32 or Float32",
        }),
    };
}

fn eval_math(name: &BuiltInFunc, env: &Environment) -> Result<Value, EvalError> {
    let x = env.get(&"lhs".into()).unwrap();
    let y = env.get(&"rhs".into()).unwrap();

    match (name, x, y) {
        (BuiltInFunc::Add, Value::Int32(x), Value::Int32(y)) => Ok((x + y).into()),
        (BuiltInFunc::Add, Value::Float32(x), Value::Float32(y)) => Ok((x + y).into()),
        (BuiltInFunc::Sub, Value::Int32(x), Value::Int32(y)) => Ok((x - y).into()),
        (BuiltInFunc::Sub, Value::Float32(x), Value::Float32(y)) => Ok((x - y).into()),
        (BuiltInFunc::Mul, Value::Int32(x), Value::Int32(y)) => Ok((x * y).into()),
        (BuiltInFunc::Mul, Value::Float32(x), Value::Float32(y)) => Ok((x * y).into()),
        (BuiltInFunc::Div, Value::Int32(x), Value::Int32(y)) => Ok((x / y).into()),
        (BuiltInFunc::Div, Value::Float32(x), Value::Float32(y)) => Ok((x / y).into()),
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

fn eval_comparison(name: &BuiltInFunc, env: &Environment) -> Result<Value, EvalError> {
    let x = env.get(&"lhs".into()).unwrap();
    let y = env.get(&"rhs".into()).unwrap();

    match (name, x, y) {
        (BuiltInFunc::Eq, Value::Int32(x), Value::Int32(y)) => Ok((x == y).into()),
        (BuiltInFunc::Eq, Value::Float32(x), Value::Float32(y)) => Ok((x == y).into()),
        (BuiltInFunc::Eq, Value::Bool(x), Value::Bool(y)) => Ok((x == y).into()),
        (BuiltInFunc::Gt, Value::Int32(x), Value::Int32(y)) => Ok((x > y).into()),
        (BuiltInFunc::Gt, Value::Float32(x), Value::Float32(y)) => Ok((x > y).into()),
        (BuiltInFunc::Lt, Value::Int32(x), Value::Int32(y)) => Ok((x < y).into()),
        (BuiltInFunc::Lt, Value::Float32(x), Value::Float32(y)) => Ok((x < y).into()),
        (BuiltInFunc::Gte, Value::Int32(x), Value::Int32(y)) => Ok((x >= y).into()),
        (BuiltInFunc::Gte, Value::Float32(x), Value::Float32(y)) => Ok((x >= y).into()),
        (BuiltInFunc::Lte, Value::Int32(x), Value::Int32(y)) => Ok((x <= y).into()),
        (BuiltInFunc::Lte, Value::Float32(x), Value::Float32(y)) => Ok((x <= y).into()),
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

fn eval_concat(env: &Environment) -> Result<Value, EvalError> {
    let x: String = env.get(&"lhs".into()).unwrap().clone().try_into()?;
    let y: String = env.get(&"rhs".into()).unwrap().clone().try_into()?;

    Ok((format!("{x}{y}")).into())
}

fn eval_builtin_function(name: &BuiltInFunc, env: &Environment) -> Result<Value, EvalError> {
    match name {
        BuiltInFunc::Abs => eval_abs(env),
        BuiltInFunc::Concat => eval_concat(env),
        BuiltInFunc::Eq
        | BuiltInFunc::Gt
        | BuiltInFunc::Lt
        | BuiltInFunc::Gte
        | BuiltInFunc::Lte => eval_comparison(name, env),
        BuiltInFunc::Add | BuiltInFunc::Sub | BuiltInFunc::Mul | BuiltInFunc::Div => {
            eval_math(name, env)
        }
    }
}
