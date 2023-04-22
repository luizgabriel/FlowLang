use crate::error::EvalError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    Unit(),
    Bool(bool),
    Int32(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int32,
    Bool,
}

impl From<i32> for LiteralValue {
    fn from(value: i32) -> Self {
        LiteralValue::Int32(value.into())
    }
}

impl From<bool> for LiteralValue {
    fn from(value: bool) -> Self {
        LiteralValue::Bool(value.into())
    }
}

impl Into<Type> for LiteralValue {
    fn into(self) -> Type {
        match self {
            LiteralValue::Unit() => Type::Unit,
            LiteralValue::Bool(_) => Type::Bool,
            LiteralValue::Int32(_) => Type::Int32,
        }
    }
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
pub enum Expr {
    Literal(LiteralValue),
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
    BuiltInFunction {
        name: BuiltInFunc,
        arity: usize,
        args: Vec<Expr>,
    },
    Lambda {
        params: Vec<Ident>,
        body: Box<Expr>,
    },
}

impl Expr {
    pub fn unit() -> Expr {
        Expr::Literal(LiteralValue::Unit())
    }

    pub fn literal<T>(value: T) -> Expr
    where
        LiteralValue: From<T>,
    {
        Expr::Literal(LiteralValue::from(value))
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

    pub fn builtin_fn(name: BuiltInFunc, arity: usize) -> Expr {
        Expr::BuiltInFunction {
            name,
            arity,
            args: Vec::new(),
        }
    }
}

macro_rules! define_try_from_expr {
    ($type:tt, $native_type:ty) => {
        impl TryFrom<Expr> for $native_type {
            type Error = EvalError;

            fn try_from(expr: Expr) -> Result<Self, Self::Error> {
                match expr {
                    Expr::Literal(LiteralValue::$type(value)) => Ok(value),
                    _ => Err(EvalError::InvalidType {
                        expr,
                        expected: Type::$type,
                    }),
                }
            }
        }
    };
}

define_try_from_expr!(Int32, i32);
define_try_from_expr!(Bool, bool);

macro_rules! define_intro_expr {
    ($native_type:ty) => {
        impl Into<Expr> for $native_type {
            fn into(self) -> Expr {
                Expr::literal(self)
            }
        }
    };
}

define_intro_expr!(i32);
define_intro_expr!(bool);
