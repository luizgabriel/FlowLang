use colored::Colorize;
use lang::ast::Expr;

pub struct ColoredExpr {
    pub expr: Expr,
}

impl ColoredExpr {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

impl std::fmt::Display for ColoredExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.expr {
            Expr::BinOp(op, lhs, rhs) => write!(
                f,
                "({} {} {})",
                ColoredExpr { expr: *lhs.clone() },
                op,
                ColoredExpr { expr: *rhs.clone() }
            ),
            Expr::UnOp(op, expr) => write!(
                f,
                "({} {})",
                op,
                ColoredExpr {
                    expr: *expr.clone()
                }
            ),
            Expr::Literal(value) => match value {
                lang::ast::LiteralValue::Bool(value) => {
                    write!(f, "{}", value.to_string().bright_red())
                }
                lang::ast::LiteralValue::Int32(value) => {
                    write!(f, "{}", value.to_string().bright_yellow())
                }
            },
            Expr::Identifier(ident) => write!(f, "{}", ident.to_string().bright_blue()),
        }
    }
}
