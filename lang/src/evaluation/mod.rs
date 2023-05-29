pub use crate::parsing::data::{Expr, Ident, Program};
pub use crate::evaluation::data::Value;
use crate::evaluation::env::Environment;
pub use crate::evaluation::env::ValueEnvironment;
pub use crate::evaluation::error::EvalError;
use crate::parsing::data::{Declaration, Statement};

mod builtin;
pub mod data;
pub mod env;
pub mod error;
mod display;

pub trait Evaluator {
    type Output;
    type Context;
    type Error;

    fn eval(&self, context: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error>;
}

impl Evaluator for Declaration {
    type Output = Value;
    type Context = ValueEnvironment;
    type Error = EvalError;

    fn eval(&self, env: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        match self {
            Declaration::Constant { name, expr } => {
                let (value, env) = expr.eval(env)?;
                Ok((Value::Unit, env.set(name.clone(), value)))
            }
            Declaration::Function { name, params, body } => {
                let function = Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    scope: env.clone(),
                };
                Ok((Value::Unit, env.set(name.clone(), function)))
            }
        }
    }
}

impl Evaluator for Statement {
    type Output = Value;
    type Context = ValueEnvironment;
    type Error = EvalError;

    fn eval(&self, env: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        match self {
            Statement::Expression(expr) => expr.eval(env),
            Statement::Declaration(decl) => decl.eval(env),
            Statement::LetBlock {
                bindings,
                body
            } => {
                let initial_env = env.clone();
                let (_, env) = bindings.iter().try_fold((Value::Unit, env), |(_, env), statement| statement.eval(env))?;
                let (value, _) = body.eval(env)?;
                Ok((value, initial_env))
            }
        }
    }
}

impl Evaluator for Expr {
    type Output = Value;
    type Context = ValueEnvironment;
    type Error = EvalError;

    fn eval(&self, env: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        match self {
            Expr::Unit => env.pure(Value::Unit),
            Expr::Bool(value) => env.pure((*value).into()),
            Expr::Int32(value) => env.pure((*value).into()),
            Expr::Float32(value) => env.pure((*value).into()),
            Expr::String(value) => env.pure(value.clone().into()),

            Expr::Identifier(ident) => {
                let value = env
                    .get(ident)
                    .ok_or_else(|| EvalError::UnboundIdentifier(ident.clone()))?;

                env.pure(value.clone())
            }

            Expr::Lambda { params, body } => {
                let function = Value::Function {
                    params: params.clone(),
                    body: Box::new(Statement::Expression(body.clone())),
                    scope: env.clone(),
                };

                env.pure(function)
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
                        let scope = scope.set(param, rhs);

                        if rest.is_empty() {
                            let (value, _) = name.eval(scope)?;
                            return env.pure(value);
                        }

                        env.pure(Value::BuiltInFunction {
                            name,
                            params: rest.iter().cloned().collect(),
                            scope,
                        })
                    }

                    Value::Function {
                        params,
                        body,
                        scope,
                    } => {
                        let (param, rest) = params.split_first().unwrap();
                        let scope = scope.set(param, rhs);

                        if rest.is_empty() {
                            let (result, _) = body.eval(scope)?;
                            return Ok((result, env));
                        }

                        env.pure(Value::Function {
                            params: rest,
                            body,
                            scope,
                        })
                    }

                    _ => Err(EvalError::NotAFunction(lhs)),
                }
            }

            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let (condition, env) = condition.eval(env)?;

                if condition.try_into()? {
                    Self::eval(then, env)
                } else {
                    Self::eval(otherwise, env)
                }
            }
        }
    }
}

impl Evaluator for Program {
    type Output = Value;
    type Context = ValueEnvironment;
    type Error = EvalError;

    fn eval(&self, env: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        self.iter().fold(Ok((Value::Unit, env)), |acc, expr| {
            let (_, env) = acc?;
            expr.eval(env)
        })
    }
}


