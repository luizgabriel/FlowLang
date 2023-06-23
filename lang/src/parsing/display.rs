use std::fmt::{Display, Formatter, Result};

use crate::parsing::data::{ParamsList, Statement};
use crate::parsing::{Expr, Ident};

use super::data::ModuleName;

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            Ident::Name(name) => write!(f, "{}", name),
            Ident::Operator(op) => write!(f, "({})", op),
        }
    }
}

impl Display for ParamsList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
            Expr::FunctionApplication(lhs, rhs) => match *rhs.clone() {
                Expr::FunctionApplication(_, _) => write!(f, "{} ({})", lhs, rhs),
                _ => write!(f, "{} {}", lhs, rhs),
            },
            Expr::Lambda { params, body } => write!(f, "({} -> {})", params, body),
            Expr::If {
                condition,
                then,
                otherwise,
            } => write!(f, "if ({}) then ({}) else ({})", condition, then, otherwise),
        }
    }
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(".")
        )
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::UseModule(module) => write!(f, "use {}", module),
            Statement::Expression(e) => write!(f, "{}", e),
            Statement::ConstantDeclaration { name, expr: value } => {
                write!(f, "{} = {}", name, value)
            }
            Statement::FunctionDeclaration { name, params, body } => {
                write!(f, "{} {} = {}", name, params, body)
            }
            Statement::Block(statements) => write!(
                f,
                "{{\n\t{}\n\t}}",
                statements
                    .iter()
                    .map(|decl| decl.to_string())
                    .collect::<Vec<String>>()
                    .join(";\n\t\t"),
            ),
        }
    }
}
