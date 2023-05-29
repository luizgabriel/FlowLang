use std::fmt::{Display, Formatter};
use colored::Colorize;
use lang::evaluation::Value;
use lang::parsing::{Bindings, Declaration, Expr, Ident, ParamsList, Program, Statement};

pub struct Colored<T> {
    value: T,
}

impl<T> Colored<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }
}

impl Display for Colored<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Expr::Unit => write!(f, "{}", "()".white()),
            Expr::Int32(value) => write!(f, "{}", value.to_string().bright_blue()),
            Expr::Float32(value) => write!(f, "{}", value.to_string().bright_blue()),
            Expr::Bool(value) => write!(f, "{}", value.to_string().red()),
            Expr::Identifier(ident) => {
                match ident {
                    Ident::Name(name) => write!(f, "{}", name.to_string().yellow()),
                    Ident::Operator(op) => write!(f, "({})", op.to_string().bright_yellow()),
                }
            },
            Expr::String(value) => write!(f, "{}", format!("\"{value}\"").green()),
            Expr::FunctionApplication(func, arg) => {
                match *arg.clone() {
                    Expr::FunctionApplication(_, _) => write!(f, "{} ({})", Colored::new(*func.clone()), Colored::new(*arg.clone())),
                    _ => write!(f, "{} {}", Colored::new(*func.clone()), Colored::new(*arg.clone())),
                }
            }
            Expr::Lambda { params, body } => write!(f, "({} -> {})", Colored::new(params.clone()), Colored::new(*body.clone())),
            Expr::If { condition, then, otherwise } => write!(f, "if {} then {} else {}", Colored::new(*condition.clone()), Colored::new(*then.clone()), Colored::new(*otherwise.clone())),
        }
    }
}

impl Display for Colored<Declaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Declaration::Function { name, params, body } => write!(f, "{} {} = {}",
                                                                   name.to_string().bright_blue(),
                                                                   Colored::new(params.clone()),
                                                                   Colored::new(*body.clone())),
            Declaration::Constant { name, expr } => write!(f, "{} = {}", name.to_string().white(), Colored::new(*expr.clone())),
        }
    }
}

impl Display for Colored<Bindings> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.value.len() == 1 {
            return write!(f, "{}", Colored::new(self.value[0].clone()));
        }

        write!(f, "\t{}", self.value
            .iter()
            .map(|decl| Colored::new(decl.clone()).to_string())
            .collect::<Vec<String>>()
            .join("\n\t"))
    }
}

impl Display for Colored<Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Statement::Expression(expr) => write!(f, "{}", Colored::new(*expr.clone())),
            Statement::Declaration(decl) => write!(f, "{}",  Colored::new(*decl.clone())),
            Statement::LetBlock { bindings, body } => {
                if bindings.len() == 1 {
                    return write!(f, "let {} then {}", Colored::new(bindings.clone()), Colored::new(*body.clone()));
                }

                write!(f, "let\n{}\n\n\tthen\n\t{}", Colored::new(bindings.clone()), Colored::new(*body.clone()))
            },
        }
    }
}

impl Display for Colored<ParamsList> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<String>>()
            .join(" ")
            .yellow())
    }
}

impl Display for Colored<Value> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Unit => write!(f, "{}", "()".white()),
            Value::Bool(value) => write!(f, "{}", value.to_string().red()),
            Value::Int32(value) => write!(f, "{}", value.to_string().cyan()),
            Value::Float32(value) => write!(f, "{}", value.to_string().cyan()),
            Value::String(value) => write!(f, "{}", format!("\"{value}\"").green()),
            Value::Function { params, body, scope: _ } => write!(f, "({} -> {})", Colored::new(params.clone()), Colored::new(*body.clone())),
            Value::BuiltInFunction { name, params, scope: _ } => write!(f, "({} -> {})", Colored::new(params.clone()), name.to_string().magenta()),
        }
    }
}

impl Display for Colored<Program> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value
            .iter()
            .map(|stmt| Colored::new(stmt.clone()).to_string())
            .collect::<Vec<String>>()
            .join("\n"))
    }
}