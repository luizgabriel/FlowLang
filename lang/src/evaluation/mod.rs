use std::borrow::Cow;
use std::{cell::RefCell, sync::Arc};

pub use crate::evaluation::data::Value;
pub use crate::evaluation::env::ValueEnvironment;
pub use crate::evaluation::error::EvalError;
pub use crate::parsing::data::{Expr, Ident, Program};
use crate::parsing::data::{ModuleName, Statement};
use crate::parsing::parse_program;
use crate::Environment;

mod builtin;
pub mod data;
mod display;
pub mod env;
pub mod error;

pub trait Evaluator {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError>;
}

impl Evaluator for ModuleName {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
        let module = match self.to_string().as_str() {
            "std" => Cow::Borrowed(include_str!("../../lib/std.fw")),
            _ => match std::fs::read_to_string(self.as_path()) {
                Ok(content) => Cow::Owned(content),
                Err(err) => return Err(EvalError::CouldNotReadModule(self.clone(), err)),
            },
        };

        parse_program(module.as_ref())
            .map_err(|e| EvalError::InvalidModule(self.clone(), e))
            .and_then(|program| program.eval(env))
    }
}

impl Evaluator for Statement {
    fn eval(&self, env: ValueEnvironment) -> Result<(Value, ValueEnvironment), EvalError> {
        match self {
            Statement::Import(module) => module.eval(env),
            Statement::Expression(expr) => expr.eval(env),
            Statement::ConstantDeclaration { name, expr } => {
                let (value, env) = expr.eval(env)?;
                Ok((Value::Unit, env.set(name.clone(), value)))
            }
            Statement::FunctionDeclaration { name, params, body } => {
                let scope = Arc::new(RefCell::new(env.clone()));

                let function = Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    scope: scope.clone(),
                };

                scope.replace(env.set(name.clone(), function));

                let new_env = scope.borrow();
                Ok((Value::Unit, new_env.clone()))
            }
            Statement::Block(statements) => {
                let (last_value, _) = statements
                    .iter()
                    .try_fold((Value::Unit, env.clone()), |(_, env), statement| {
                        statement.eval(env)
                    })?;

                Ok((last_value, env))
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
                    scope: Arc::new(env.clone().into()),
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
                            scope: Arc::new(scope.into()),
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
