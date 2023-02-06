#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Bool(bool),
    Int32(i32),
}

impl From<i32> for LiteralValue
{
    fn from(value: i32) -> Self {
        LiteralValue::Int32(value.into())
    }
}

impl From<bool> for LiteralValue
{
    fn from(value: bool) -> Self {
        LiteralValue::Bool(value.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(String);

impl Ident {
    pub fn new(name: &str) -> Self {
        Ident(name.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Pow, // ^
    Eq,  // =
    Not, // !
    Gt, // >
    Lt, // <
    Gte, // >=
    Lte, // <=
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinOp(OpKind, Box<Expr>, Box<Expr>),
    UnOp(OpKind, Box<Expr>),
    Literal(LiteralValue),
    Identifier(Ident),
}

impl Expr {
    pub fn literal<T>(value: T) -> Expr
    where
        LiteralValue: From<T>,
    {
        Expr::Literal(LiteralValue::from(value))
    }

    pub fn ident(name: &str) -> Expr {
        Expr::Identifier(Ident::new(name))
    }

    pub fn binop(op: OpKind, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinOp(op, Box::new(lhs), Box::new(rhs))
    }

    pub fn unop(op: OpKind, expr: Expr) -> Expr {
        Expr::UnOp(op, Box::new(expr))
    }
}
