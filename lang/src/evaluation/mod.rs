use std::{cell::RefCell, rc::Rc};

pub use crate::evaluation::data::Value;
use crate::Environment;
pub use crate::evaluation::env::ValueEnvironment;
pub use crate::evaluation::error::EvalError;
use crate::parsing::data::Statement;
pub use crate::parsing::data::{Expr, Ident, Program};

mod builtin;
pub mod data;
mod display;
pub mod env;
pub mod error;

pub trait Evaluator {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError>;
}

impl Evaluator for Statement {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
        match self {
            Statement::Expression(expr) => expr.eval(env),
            Statement::ConstantDeclaration { name, expr } => {
                let (value, env) = expr.eval(env)?;
                Ok((Value::Unit, env.set(name.clone(), value)))
            }
            Statement::FunctionDeclaration { name, params, body } => {
                let scope = Rc::new(RefCell::new(env.clone()));

                let function = Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    scope: scope.clone(),
                };

                scope.replace(env.set(name.clone(), function));
                let new_env = scope.borrow();

                Ok((Value::Unit, new_env.clone()))
            }
            Statement::Block { statements, body } => {
                let initial_env = env.clone();
                let (_, env) = statements
                    .iter()
                    .try_fold((Value::Unit, env), |(_, env), statement| {
                        statement.eval(env)
                    })?;
                let (value, _) = body.eval(env)?;
                Ok((value, initial_env))
            }
        }
    }
}

impl Evaluator for Expr {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
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
                    body: body.clone().into(),
                    scope: Rc::new(env.clone().into()),
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
                            params: rest,
                            scope,
                        })
                    }

                    Value::Function {
                        params,
                        body,
                        scope,
                    } => {
                        let (param, rest) = params.split_first().unwrap();
                        let scope = scope.borrow().set(param, rhs);

                        if rest.is_empty() {
                            let (result, _) = body.eval(scope)?;
                            return Ok((result, env));
                        }

                        env.pure(Value::Function {
                            params: rest,
                            body,
                            scope: Rc::new(scope.into()),
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
                    then.eval(env)
                } else {
                    otherwise.eval(env)
                }
            }
        }
    }
}

impl Evaluator for Program {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
        self.iter().fold(Ok((Value::Unit, env)), |acc, expr| {
            let (_, env) = acc?;
            expr.eval(env)
        })
    }
}
