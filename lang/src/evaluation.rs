use std::iter::once;

use rpds::HashTrieMap;

use crate::{
    ast::{BuiltInFunc, Expr, Ident},
    error::EvalError,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    variables: HashTrieMap<String, Expr>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashTrieMap::new(),
        }
    }

    pub fn get_id(&self, identifier: &Ident) -> Option<&Expr> {
        self.get(identifier.0.as_str())
    }

    pub fn get(&self, identifier: &str) -> Option<&Expr> {
        self.variables.get(&identifier.to_string())
    }

    pub fn set(&self, identifier: &str, value: Expr) -> Self {
        Environment {
            variables: self.variables.insert(identifier.to_string(), value),
        }
    }

    pub fn set_id(&self, identifier: &Ident, value: Expr) -> Self {
        self.set(identifier.0.as_str(), value)
    }

    pub fn new_with_std() -> Self {
        Self::new()
            .set("+", Expr::builtin_fn(BuiltInFunc::Add, 2))
            .set("-", Expr::builtin_fn(BuiltInFunc::Sub, 2))
            .set("*", Expr::builtin_fn(BuiltInFunc::Mul, 2))
            .set("/", Expr::builtin_fn(BuiltInFunc::Div, 2))
    }
}

type EvalResult = Result<(Expr, Environment), EvalError>;

pub fn eval(expr: &Expr, env: Environment) -> EvalResult {
    match expr {
        Expr::Identifier(ident) => {
            let value = env
                .get_id(ident)
                .ok_or(EvalError::UnboundIdentifier(ident.clone()))?;

            Ok((value.clone(), env))
        }

        Expr::ConstantDefinition { name, value } => {
            Ok((Expr::unit(), env.set_id(name, *value.clone())))
        }

        Expr::FunctionApplication(lhs, rhs) => {
            let (lhs, env) = lhs.eval(env)?;
            let (rhs, env) = rhs.eval(env)?;

            if let Expr::BuiltInFunction { name, arity, args } = lhs {
                let new_args: Vec<_> = args.iter().chain(once(&rhs)).cloned().collect();
                if new_args.len() < arity {
                    return Ok((
                        Expr::BuiltInFunction {
                            name,
                            arity,
                            args: new_args,
                        },
                        env,
                    ));
                }

                return eval_builtin_function(name, new_args, env);
            }

            let (body, param) = match lhs {
                Expr::Lambda { params, body } => (body, params.first().cloned().unwrap()),
                Expr::FunctionDefinition {
                    name: _,
                    params,
                    body,
                } => (body, params.first().cloned().unwrap()),
                _ => return Err(EvalError::NotAFunction(lhs.clone())),
            };

            let (result, _) = body.eval(env.clone().set_id(&param, rhs))?;
            Ok((result, env))
        }

        Expr::FunctionDefinition {
            name,
            params: _,
            body: _,
        } => Ok((Expr::unit(), env.set_id(name, expr.clone()))),

        _ => Ok((expr.clone(), env)),
    }
}

fn eval_builtin_function(name: BuiltInFunc, args: Vec<Expr>, env: Environment) -> EvalResult {
    let (lhs, rhs) = (
        i32::try_from(args.first().unwrap().clone())?,
        i32::try_from(args.last().unwrap().clone())?,
    );

    match name {
        BuiltInFunc::Add => Ok(((lhs + rhs).into(), env)),
        BuiltInFunc::Sub => Ok(((lhs - rhs).into(), env)),
        BuiltInFunc::Mul => Ok(((lhs * rhs).into(), env)),
        BuiltInFunc::Div => Ok(((lhs / rhs).into(), env)),
    }
}

impl Expr {
    pub fn eval(&self, env: Environment) -> EvalResult {
        eval(self, env)
    }
}
