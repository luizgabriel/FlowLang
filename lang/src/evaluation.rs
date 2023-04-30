use crate::{
    ast::{BuiltInFunc, Expr, Type, Value},
    env::Environment,
    error::EvalError,
};

type EvalResult = Result<(Value, Environment), EvalError>;

pub fn eval(expr: &Expr, env: Environment) -> EvalResult {
    match expr {
        Expr::Literal(value) => Ok((value.clone(), env)),

        Expr::Identifier(ident) => {
            let value = env
                .get(ident)
                .ok_or_else(|| EvalError::UnboundIdentifier(ident.clone()))?;

            Ok((value.clone(), env))
        }

        Expr::Lambda { params, body } => {
            let function = Value::Function {
                params: params.clone(),
                body: body.clone(),
                scope: env.clone(),
            };

            Ok((function, env))
        }

        Expr::ConstantDefinition { name, value } => {
            let (value, env) = value.eval(env)?;
            Ok((Value::Unit(), env.set(name.clone(), value)))
        }

        Expr::FunctionApplication(lhs, rhs) => {
            let (lhs, env) = lhs.eval(env)?;
            let (rhs, env) = rhs.eval(env)?;

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
                        let (result, _) = body.eval(scope)?;
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
                params: params.clone(),
                body: body.clone(),
                scope: env.clone(),
            };

            Ok((Value::Unit(), env.set(name.clone(), function)))
        }

        Expr::If {
            condition,
            then,
            otherwise,
        } => {
            let (condition, env) = condition.eval(env)?;

            match condition {
                Value::Bool(true) => then.eval(env),
                Value::Bool(false) => otherwise.eval(env),
                _ => Err(EvalError::InvalidType {
                    value: condition.clone(),
                    expected: Type::Bool,
                }),
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
            expected: Type::Int32,
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
            expected: Type::Int32,
        }),
        (_, Value::Float32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: Type::Float32,
        }),
        (_, x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: Type::Int32,
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
            expected: Type::Int32,
        }),
        (_, Value::Float32(_), y) => Err(EvalError::InvalidType {
            value: y.clone(),
            expected: Type::Float32,
        }),
        (_, x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: Type::Int32,
        }),
    }
}

fn eval_concat(env: &Environment) -> Result<Value, EvalError> {
    let x = env.get(&"lhs".into()).unwrap();
    let y = env.get(&"rhs".into()).unwrap();

    match (x, y) {
        (Value::String(x), Value::String(y)) => Ok((format!("{x}{y}")).into()),
        (x, _) => Err(EvalError::InvalidType {
            value: x.clone(),
            expected: Type::String,
        }),
    }
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

impl Expr {
    pub fn eval(&self, env: Environment) -> EvalResult {
        eval(self, env)
    }
}
