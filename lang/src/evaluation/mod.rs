mod builtin;
pub mod data;
pub mod env;
pub mod error;
mod display;

use crate::{
    parsing::data::{Expr, Ident, Module},
};
use crate::evaluation::env::ValueEnvironment;
use crate::evaluation::error::EvalError;
use crate::evaluation::data::Value;

pub trait Environment: Sized + Clone {
    type Value;

    fn get(&self, identifier: &Ident) -> Option<&Self::Value>;
    fn set(&self, identifier: Ident, value: Self::Value) -> Self;

    fn eval<T, E>(&self, evaluator: T) -> Result<(Self::Value, Self), E>
    where
        T: Evaluator<Context = Self, Output = Self::Value, Error = E>,
    {
        let (value, next_env) = evaluator.eval(self.clone())?;
        Ok((value, next_env))
    }

    fn pure<E>(&self, value: Self::Value) -> Result<(Self::Value, Self), E> {
        Ok((value, self.clone()))
    }
}

pub trait Evaluator {
    type Output;
    type Context;
    type Error;

    fn eval(&self, context: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error>;
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
                    body: body.clone(),
                    scope: env.clone(),
                };

                env.pure(function)
            }

            Expr::ConstantDefinition { name, expr } => {
                let (value, env) = Self::eval(expr, env)?;

                Ok((Value::Unit, env.set(name.clone(), value)))
            }

            Expr::FunctionApplication(lhs, rhs) => {
                let (lhs, env) = Self::eval(lhs, env)?;
                let (rhs, env) = Self::eval(rhs, env)?;

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
                            let (result, _) = Self::eval(&body, scope)?;
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

            Expr::FunctionDefinition { name, params, body } => {
                let function = Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    scope: env.clone(),
                };

                Ok((Value::Unit, env.set(name.clone(), function)))
            }

            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let (condition, env) = Self::eval(condition, env)?;

                if condition.try_into()? {
                    Self::eval(then, env)
                } else {
                    Self::eval(otherwise, env)
                }
            }
        }
    }
}

impl Evaluator for Module {
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


