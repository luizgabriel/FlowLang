use std::fmt::{Display, Formatter, Result};

use crate::parsing::data::{Declaration, ParamsList, Statement};
use crate::parsing::{Bindings, Expr, Ident};

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
            Expr::Lambda { params, body } => write!(
                f,
                "({} -> {})",
                params,
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

impl Display for Bindings {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.len() == 1 {
            return write!(f, "{}", self[0]);
        }

        write!(f, "\t{}", self
            .iter()
            .map(|decl| decl.to_string())
            .collect::<Vec<String>>()
            .join("\n\t"))
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Expression(e) => write!(f, "{}", e),
            Statement::Declaration(decl) => write!(f, "{}", decl),
            Statement::LetBlock {
                bindings,
                body,
            } => {
                if bindings.len() == 1 {
                    return write!(f, "let {} then {}", bindings, body);
                }

                write!(f, "let\n{}\n\n\tthen\n\t{}", bindings, *body.clone())
            }
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Declaration::Constant { name, expr: value } => write!(f, "{} = {}", name, value),
            Declaration::Function { name, params, body } => {
                write!(f, "{} {} = {}", name, params, body)
            }
        }
    }
}

