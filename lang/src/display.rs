use std::fmt::{Display, Error, Formatter};

use crate::ast::{Expr, Ident, LiteralValue};

impl Display for LiteralValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LiteralValue::Unit() => write!(f, "()"),
            LiteralValue::Bool(value) => write!(f, "{}", value),
            LiteralValue::Int32(value) => write!(f, "{}", value),
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Identifier(ident) => write!(f, "{}", ident),
            Expr::ConstantDefinition { name, value } => write!(f, "{} = {}", name, value),
            Expr::FunctionDefinition { name, params, body } => write!(
                f,
                "{} {} = {}",
                name,
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
                body
            ),
            Expr::FunctionApplication(lhs, rhs) => match (*lhs.clone(), *rhs.clone()) {
                (_, Expr::FunctionApplication(_, _)) => {
                    write!(f, "{} ({})", lhs, rhs)
                }
                (_, _) => write!(f, "{} {}", lhs, rhs),
            },
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
            Expr::BuiltInFunction { name, arity, args } => write!(
                f,
                "builtin <{}> {:?} [{}]",
                arity,
                name,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
            ),
        }
    }
}
