use rpds::HashTrieMap;

use crate::{
    ast::{BuiltInFunc, Expr, Ident, Value},
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
            .set(
                Ident::new("+"),
                Value::BuiltInFunction {
                    name: BuiltInFunc::Add,
                    params: vec![Ident::new("x"), Ident::new("y")],
                    scope: Environment::new(),
                },
            )
            .set(
                Ident::new("-"),
                Value::BuiltInFunction {
                    name: BuiltInFunc::Sub,
                    params: vec![Ident::new("x"), Ident::new("y")],
                    scope: Environment::new(),
                },
            )
            .set(
                Ident::new("*"),
                Value::BuiltInFunction {
                    name: BuiltInFunc::Mul,
                    params: vec![Ident::new("x"), Ident::new("y")],
                    scope: Environment::new(),
                },
            )
            .set(
                Ident::new("/"),
                Value::BuiltInFunction {
                    name: BuiltInFunc::Div,
                    params: vec![Ident::new("x"), Ident::new("y")],
                    scope: Environment::new(),
                },
            )
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
    let x: i32 = env.get(&Ident::new("x"))?.clone().try_into()?;
    let y: i32 = env.get(&Ident::new("y"))?.clone().try_into()?;

    match name {
        BuiltInFunc::Add => Ok((x + y).into()),
        BuiltInFunc::Sub => Ok((x - y).into()),
        BuiltInFunc::Mul => Ok((x * y).into()),
        BuiltInFunc::Div => Ok((x / y).into()),
    }
}

impl Expr {
    pub fn eval(&self, env: Environment) -> EvalResult {
        eval(self, env)
    }
}
