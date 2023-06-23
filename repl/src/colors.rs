use colored::Colorize;
use lang::evaluation::Value;
use lang::parsing::data::ModuleName;
use lang::parsing::{Expr, Ident, ParamsList, Program, Statement};
use std::fmt::{Display, Formatter, Result};

pub struct Colored<T> {
    value: T,
}

impl<T> Colored<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }
}

impl Display for Colored<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.value {
            Expr::Unit => write!(f, "{}", "()".white()),
            Expr::Int32(value) => write!(f, "{}", value.to_string().bright_blue()),
            Expr::Float32(value) => write!(f, "{}", value.to_string().bright_blue()),
            Expr::Bool(value) => write!(f, "{}", value.to_string().red()),
            Expr::Identifier(ident) => match ident {
                Ident::Name(name) => write!(f, "{}", name.to_string().yellow()),
                Ident::Operator(op) => write!(f, "({})", op.to_string().bright_yellow()),
            },
            Expr::String(value) => write!(f, "{}", format!("\"{value}\"").green()),
            Expr::FunctionApplication(func, arg) => match *arg.clone() {
                Expr::FunctionApplication(_, _) => write!(
                    f,
                    "{} ({})",
                    Colored::new(*func.clone()),
                    Colored::new(*arg.clone())
                ),
                _ => write!(
                    f,
                    "{} {}",
                    Colored::new(*func.clone()),
                    Colored::new(*arg.clone())
                ),
            },
            Expr::Lambda { params, body } => write!(
                f,
                "({} -> {})",
                Colored::new(params.clone()),
                Colored::new(*body.clone())
            ),
            Expr::If {
                condition,
                then,
                otherwise,
            } => write!(
                f,
                "if {} then {} else {}",
                Colored::new(*condition.clone()),
                Colored::new(*then.clone()),
                Colored::new(*otherwise.clone())
            ),
        }
    }
}

impl Display for Colored<ModuleName> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.value
                .iter()
                .map(|s| s.to_string().bright_yellow().to_string())
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

impl Display for Colored<Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.value {
            Statement::Import(module_name) => {
                write!(f, "use {}", Colored::new(module_name.clone()))
            }
            Statement::Expression(expr) => write!(f, "{}", Colored::new(expr.clone())),
            Statement::FunctionDeclaration { name, params, body } => write!(
                f,
                "{} {} = {}",
                name.to_string().bright_blue(),
                Colored::new(params.clone()),
                Colored::new(*body.clone())
            ),
            Statement::ConstantDeclaration { name, expr } => write!(
                f,
                "{} = {}",
                name.to_string().white(),
                Colored::new(expr.clone())
            ),
            Statement::Block(statements) => {
                write!(
                    f,
                    "{{\n\t{}\n\t}}",
                    statements
                        .iter()
                        .map(|s| Colored::new(s.clone()).to_string())
                        .collect::<Vec<_>>()
                        .join(";\n\t\t"),
                )
            }
        }
    }
}

impl Display for Colored<ParamsList> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.value
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<_>>()
                .join(" ")
                .yellow()
        )
    }
}

impl Display for Colored<Value> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.value {
            Value::Unit => write!(f, "{}", "()".white()),
            Value::Bool(value) => write!(f, "{}", value.to_string().red()),
            Value::Int32(value) => write!(f, "{}", value.to_string().cyan()),
            Value::Float32(value) => write!(f, "{}", value.to_string().cyan()),
            Value::String(value) => write!(f, "{}", format!("\"{value}\"").green()),
            Value::Function {
                params,
                body,
                scope: _,
            } => write!(
                f,
                "({} -> {})",
                Colored::new(params.clone()),
                Colored::new(*body.clone())
            ),
            Value::BuiltInFunction {
                name,
                params,
                scope: _,
            } => write!(
                f,
                "({} -> {})",
                Colored::new(params.clone()),
                name.to_string().magenta()
            ),
        }
    }
}

impl Display for Colored<Program> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.value
                .iter()
                .map(|stmt| Colored::new(stmt.clone()).to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
