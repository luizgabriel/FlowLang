use rpds::HashTrieMap;

use crate::{
    ast::{BuiltInFunc, Expr, Ident, Type, Value},
    error::EvalError,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    variables: HashTrieMap<Ident, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashTrieMap::new(),
        }
    }

    pub fn get(&self, identifier: &Ident) -> Result<&Value, EvalError> {
        self.variables
            .get(identifier)
            .ok_or(EvalError::UnboundIdentifier(identifier.clone()))
    }

    pub fn set(&self, identifier: Ident, value: Value) -> Self {
        Environment {
            variables: self.variables.insert(identifier, value),
        }
    }

    pub fn new_with_std() -> Self {
        Self::new()
            .set(Ident::new("+"), Value::builtin_2(BuiltInFunc::Add))
            .set(Ident::new("-"), Value::builtin_2(BuiltInFunc::Sub))
            .set(Ident::new("*"), Value::builtin_2(BuiltInFunc::Mul))
            .set(Ident::new("/"), Value::builtin_2(BuiltInFunc::Div))
            .set(Ident::new("=="), Value::builtin_2(BuiltInFunc::Eq))
            .set(Ident::new(">"), Value::builtin_2(BuiltInFunc::Gt))
            .set(Ident::new(">="), Value::builtin_2(BuiltInFunc::Gte))
            .set(Ident::new("<"), Value::builtin_2(BuiltInFunc::Lt))
            .set(Ident::new("<="), Value::builtin_2(BuiltInFunc::Lte))
    }
}

type EvalResult = Result<(Value, Environment), EvalError>;

pub fn eval(expr: &Expr, env: Environment) -> EvalResult {
    match expr {
        Expr::Literal(value) => Ok((value.clone(), env)),

        Expr::Identifier(ident) => {
            let value = env.get(ident)?;
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
    }
}

fn eval_builtin_function(name: &BuiltInFunc, env: &Environment) -> Result<Value, EvalError> {
    let x = env.get(&Ident::new("lhs"))?;
    let y = env.get(&Ident::new("rhs"))?;

    match (name, x, y) {
        (BuiltInFunc::Add, Value::Int32(x), Value::Int32(y)) => Ok((x + y).into()),
        (BuiltInFunc::Add, Value::Float32(x), Value::Float32(y)) => Ok((x + y).into()),
        (BuiltInFunc::Sub, Value::Int32(x), Value::Int32(y)) => Ok((x - y).into()),
        (BuiltInFunc::Sub, Value::Float32(x), Value::Float32(y)) => Ok((x - y).into()),
        (BuiltInFunc::Mul, Value::Int32(x), Value::Int32(y)) => Ok((x * y).into()),
        (BuiltInFunc::Mul, Value::Float32(x), Value::Float32(y)) => Ok((x * y).into()),
        (BuiltInFunc::Div, Value::Int32(x), Value::Int32(y)) => Ok((x / y).into()),
        (BuiltInFunc::Div, Value::Float32(x), Value::Float32(y)) => Ok((x / y).into()),
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

impl Expr {
    pub fn eval(&self, env: Environment) -> EvalResult {
        eval(self, env)
    }
}
