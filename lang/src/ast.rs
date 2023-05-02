use crate::{env::Environment, error::EvalError};

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
pub enum Expr {
    Unit,
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
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
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
}

impl Expr {
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

    pub fn ife(condition: Expr, then: Expr, otherwise: Expr) -> Expr {
        Expr::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltInFunc {
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
    Add,
    Sub,
    Mul,
    Div,
    Abs,
    Concat,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit(),
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
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

impl Value {
    pub fn builtin_1(name: BuiltInFunc) -> Value {
        Value::BuiltInFunction {
            name,
            params: vec![Ident::new("x")],
            scope: Environment::new(),
        }
    }

    pub fn builtin_2(name: BuiltInFunc) -> Value {
        Value::BuiltInFunction {
            name,
            params: vec![Ident::new("lhs"), Ident::new("rhs")],
            scope: Environment::new(),
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
define_value_conversion!(String, String);
