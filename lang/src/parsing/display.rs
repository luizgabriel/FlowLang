use std::fmt::{Display, Formatter, Result};
use crate::parsing::data::ParamsList;
use crate::parsing::Expr;

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


