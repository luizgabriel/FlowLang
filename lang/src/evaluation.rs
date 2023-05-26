use rpds::HashTrieMap;
use thiserror::Error;

use crate::{
    builtin::BuiltInFunc,
    core::{Environment, Evaluator},
    params,
    parsing::{parse_module, Expr, Ident, Module, ParamsList},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit(),
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
    Function {
        params: ParamsList,
        body: Box<Expr>,
        scope: ValueEnvironment,
    },
    BuiltInFunction {
        name: BuiltInFunc,
        params: ParamsList,
        scope: ValueEnvironment,
    },
}

impl Value {
    pub fn builtin_1(name: BuiltInFunc) -> Value {
        Value::BuiltInFunction {
            name,
            params: params!(x),
            scope: ValueEnvironment::new(),
        }
    }

    pub fn builtin_2(name: BuiltInFunc) -> Value {
        Value::BuiltInFunction {
            name,
            params: params!(lhs, rhs),
            scope: ValueEnvironment::new(),
        }
    }
}

macro_rules! define_value_conversion {
    ($type:tt, $native_type:ty) => {
        impl TryFrom<Value> for $native_type {
            type Error = EvalError;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                match value {
                    Value::$type(value) => Ok(value),
                    _ => Err(EvalError::InvalidType {
                        value,
                        expected: stringify!($type),
                    }),
                }
            }
        }

        impl From<$native_type> for Value {
            fn from(value: $native_type) -> Self {
                Value::$type(value)
            }
        }
    };
}

define_value_conversion!(Int32, i32);
define_value_conversion!(Float32, f32);
define_value_conversion!(Bool, bool);
define_value_conversion!(String, String);

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ValueEnvironment {
    variables: HashTrieMap<Ident, Value>,
}

impl Environment for ValueEnvironment {
    type Value = Value;

    fn get(&self, identifier: &Ident) -> Option<&Self::Value> {
        self.variables.get(identifier)
    }

    fn set(&self, identifier: Ident, value: Self::Value) -> Self {
        ValueEnvironment {
            variables: self.variables.insert(identifier, value),
        }
    }
}

impl ValueEnvironment {
    pub fn new() -> Self {
        Self {
            variables: HashTrieMap::new(),
        }
    }

    pub fn import_std(&self) -> Self {
        let stdlib = parse_module(
            r#"
            equalUpTo epsilon x y = abs (x - y) < epsilon
            max x y = if x > y then x else y
            min x y = if x < y then x else y
            (|>) a f = f a
            (>>) f g = x -> g (f x)
            (<<) f g = x -> f (g x)
        "#,
        )
        .expect("Could not parse Standard Library");

        self.set(Ident::new("+"), Value::builtin_2(BuiltInFunc::Add))
            .set(Ident::new("-"), Value::builtin_2(BuiltInFunc::Sub))
            .set(Ident::new("*"), Value::builtin_2(BuiltInFunc::Mul))
            .set(Ident::new("/"), Value::builtin_2(BuiltInFunc::Div))
            .set(Ident::new("=="), Value::builtin_2(BuiltInFunc::Eq))
            .set(Ident::new(">"), Value::builtin_2(BuiltInFunc::Gt))
            .set(Ident::new(">="), Value::builtin_2(BuiltInFunc::Gte))
            .set(Ident::new("<"), Value::builtin_2(BuiltInFunc::Lt))
            .set(Ident::new("<="), Value::builtin_2(BuiltInFunc::Lte))
            .set(Ident::new("++"), Value::builtin_2(BuiltInFunc::Concat))
            .set(Ident::new("abs"), Value::builtin_1(BuiltInFunc::Abs))
            .set(Ident::new("sqrt"), Value::builtin_1(BuiltInFunc::Sqrt))
            .set(Ident::new("pow"), Value::builtin_2(BuiltInFunc::Pow))
            .eval(stdlib)
            .expect("Could not evaluate Standard Library")
            .1
    }
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unbound variable: {0}")]
    UnboundIdentifier(Ident),

    #[error("Not a function: {0}")]
    NotAFunction(Value),

    #[error("Invalid type: expected {expected} got {value}")]
    InvalidType {
        value: Value,
        expected: &'static str,
    },

    #[error("Division by zero")]
    DivideByZero,

    #[error("Operation Overflow")]
    Overflow,
}

impl Evaluator for Expr {
    type Output = Value;
    type Context = ValueEnvironment;
    type Error = EvalError;

    fn eval(&self, env: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error> {
        match self {
            Expr::Unit => env.pure(Value::Unit()),
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

                Ok((Value::Unit(), env.set(name.clone(), value)))
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

                Ok((Value::Unit(), env.set(name.clone(), function)))
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
        self.iter().fold(Ok((Value::Unit(), env)), |acc, expr| {
            let (_, env) = acc?;
            expr.eval(env)
        })
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Unit() => write!(f, "()"),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Int32(value) => write!(f, "{}", value),
            Value::Float32(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "\"{}\"", value),
            Value::Function {
                params,
                body,
                scope: _,
            } => write!(f, "({} -> {})", params, body,),
            Value::BuiltInFunction {
                name,
                params,
                scope: _,
            } => {
                write!(f, "({} -> {})", params, name,)
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unit => write!(f, "()"),
            Expr::Bool(value) => write!(f, "{}", value),
            Expr::Int32(value) => write!(f, "{}", value),
            Expr::Float32(value) => write!(f, "{}", value),
            Expr::String(value) => write!(f, "\"{}\"", value),
            Expr::Identifier(ident) => write!(f, "{}", ident),
            Expr::ConstantDefinition { name, expr: value } => write!(f, "{} = {}", name, value),
            Expr::FunctionApplication(lhs, rhs) => match (*lhs.clone(), *rhs.clone()) {
                (_, Expr::FunctionApplication(_, _)) => write!(f, "{} ({})", lhs, rhs),
                (_, _) => write!(f, "{} {}", lhs, rhs),
            },
            Expr::FunctionDefinition { name, params, body } => {
                write!(f, "{} {} = {}", name, params, body)
            }
            Expr::Lambda { params, body } => write!(
                f,
                "({} -> {})",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
                body
            ),
            Expr::If {
                condition,
                then,
                otherwise,
            } => write!(f, "if ({}) then ({}) else ({})", condition, then, otherwise),
        }
    }
}
