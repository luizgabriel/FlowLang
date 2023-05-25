use std::fmt::{Display, Formatter, Result};

use super::{
    ast::{Expr, Ident, Module, ParamsList},
    error::ParseError,
};

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for ParamsList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
            Expr::Lambda { params, body } => write!(f, "({} -> {})", params, body),
            Expr::If {
                condition,
                then,
                otherwise,
            } => write!(f, "if ({}) then ({}) else ({})", condition, then, otherwise),
        }
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ParseError::IncompleteInput => write!(f, "Incomplete input"),
            ParseError::ParseError(e) => write!(f, "{}", e),
        }
    }
}
