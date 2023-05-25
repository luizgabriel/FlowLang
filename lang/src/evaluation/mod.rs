use crate::parsing::ast::{Expr, Module};

use self::{
    context::{Environment, ValueEnvironment},
    error::EvalError,
    value::Value,
};

mod builtin;
pub mod context;
pub mod display;
pub mod error;
pub mod value;

pub trait Evaluator {
    type Output;
    type Context;
    type Error;

    fn eval(self, context: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error>;
}

impl Evaluator for Module {
    type Context = ValueEnvironment;
    type Error = EvalError;
    type Output = Value;

    fn eval(self, context: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        self.iter()
            .cloned()
            .try_fold((Value::Unit(), context), |(_, ctx), expr| expr.eval(ctx))
    }
}

impl Evaluator for Expr {
    type Output = Value;
    type Context = ValueEnvironment;
    type Error = EvalError;

    fn eval(self, env: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        match self {
            Expr::Unit => env.pure(Value::Unit()),
            Expr::Bool(value) => env.pure(value.into()),
            Expr::Int32(value) => env.pure(value.into()),
            Expr::Float32(value) => env.pure(value.into()),
            Expr::String(value) => env.pure(value.into()),

            Expr::Identifier(ident) => {
                let value = env
                    .get(&ident)
                    .ok_or_else(|| EvalError::UnboundIdentifier(ident))?;

                env.pure(value.clone())
            }

            Expr::Lambda { params, body } => env.pure(Value::Function {
                params,
                body,
                scope: env.clone(),
            }),

            Expr::ConstantDefinition { name, expr } => {
                let (value, env) = expr.eval(env)?;

                env.set(name.clone(), value).pure(Value::Unit())
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
                        let scope = scope.clone().set(param.clone(), rhs);

                        if rest.is_empty() {
                            let (result, _) = body.eval(scope)?;
                            return env.pure(result);
                        }

                        env.pure(Value::Function {
                            params: rest.iter().cloned().collect(),
                            body,
                            scope,
                        })
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

                env.set(name.clone(), function).pure(Value::Unit())
            }

            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let (condition, env) = condition.eval(env)?;

                if condition.try_into()? {
                    then.eval(env)
                } else {
                    otherwise.eval(env)
                }
            }
        }
    }
}
