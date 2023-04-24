use crate::{error::EvalError, evaluation::Environment};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int32,
    Float32,
    Bool,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub(crate) String);

impl Ident {
    pub fn new(name: &str) -> Self {
        Ident(name.to_string())
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Ident::new(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltInFunc {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit(),
    Bool(bool),
    Int32(i32),
    Float32(f32),
    Function {
        params: Vec<Ident>,
        body: Box<Expr>,
        scope: Environment,
    },
    BuiltInFunction {
        name: BuiltInFunc,
        params: Vec<Ident>,
        scope: Environment,
    },
}

impl Into<Type> for Value {
    fn into(self) -> Type {
        match self {
            Value::Unit() => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Int32(_) => Type::Int32,
            Value::Float32(_) => Type::Float32,
            Value::BuiltInFunction {
                name: _,
                params: _,
                scope: _,
            } => Type::Function,
            Value::Function {
                params: _,
                body: _,
                scope: _,
            } => Type::Function,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
    Identifier(Ident),
    ConstantDefinition {
        name: Ident,
        value: Box<Expr>,
    },
    FunctionApplication(Box<Expr>, Box<Expr>),
    FunctionDefinition {
        name: Ident,
        params: Vec<Ident>,
        body: Box<Expr>,
    },
    Lambda {
        params: Vec<Ident>,
        body: Box<Expr>,
    },
}

impl Expr {
    pub fn unit() -> Expr {
        Expr::Literal(Value::Unit())
    }

    pub fn literal<T>(value: T) -> Expr
    where
        T: Into<Value>,
    {
        Expr::Literal(value.into())
    }

    pub fn ident(name: &str) -> Expr {
        Expr::Identifier(Ident::new(name))
    }

    pub fn constdef(name: Ident, value: Expr) -> Expr {
        Expr::ConstantDefinition {
            name,
            value: Box::new(value),
        }
    }

    pub fn fndef(name: Ident, params: Vec<Ident>, body: Expr) -> Expr {
        Expr::FunctionDefinition {
            name,
            params,
            body: Box::new(body),
        }
    }

    pub fn fnapp(func: Expr, arg: Expr) -> Expr {
        Expr::FunctionApplication(Box::new(func), Box::new(arg))
    }

    pub fn lambda(params: Vec<Ident>, body: Expr) -> Expr {
        Expr::Lambda {
            params,
            body: Box::new(body),
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
                        expected: Type::$type,
                    }),
                }
            }
        }

        impl Into<Value> for $native_type {
            fn into(self) -> Value {
                Value::$type(self)
            }
        }
    };
}

define_value_conversion!(Int32, i32);
define_value_conversion!(Float32, f32);
define_value_conversion!(Bool, bool);
