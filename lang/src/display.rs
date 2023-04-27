use std::fmt::{Display, Error, Formatter};

use crate::ast::{Expr, Ident, Type, Value};

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
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
            } => write!(
                f,
                "({} -> {})",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
                body,
            ),
            Value::BuiltInFunction {
                name,
                params,
                scope: _,
            } => {
                write!(
                    f,
                    "({} -> <builtin-{:?}>)",
                    params
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<String>>()
                        .join(" "),
                    name,
                )
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Identifier(ident) => write!(f, "{}", ident),
            Expr::ConstantDefinition { name, value } => write!(f, "{} = {}", name, value),
            Expr::FunctionApplication(lhs, rhs) => match (*lhs.clone(), *rhs.clone()) {
                (_, Expr::FunctionApplication(_, _)) => write!(f, "{} ({})", lhs, rhs),
                (_, _) => write!(f, "{} {}", lhs, rhs),
            },
            Expr::FunctionDefinition { name, params, body } => {
                write!(
                    f,
                    "{} {} = {}",
                    name,
                    params
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<String>>()
                        .join(" "),
                    body
                )
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

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "Bool"),
            Type::Int32 => write!(f, "Int32"),
            Type::Float32 => write!(f, "Float32"),
            Type::Function => write!(f, "Function"),
            Type::String => write!(f, "String"),
        }
    }
}
