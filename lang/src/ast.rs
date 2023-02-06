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
    Assign,  // =
    Not, // !
    Gt, // >
    Lt, // <
    Gte, // >=
    Lte, // <=
    Eq, // ==
    NotEq, // !=
    Comma, // ,
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

impl std::fmt::Display for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OpKind::Add => write!(f, "+"),
            OpKind::Sub => write!(f, "-"),
            OpKind::Mul => write!(f, "*"),
            OpKind::Div => write!(f, "/"),
            OpKind::Pow => write!(f, "^"),
            OpKind::Assign => write!(f, "="),
            OpKind::Not => write!(f, "!"),
            OpKind::Gt => write!(f, ">"),
            OpKind::Lt => write!(f, "<"),
            OpKind::Gte => write!(f, ">="),
            OpKind::Lte => write!(f, "<="),
            OpKind::Eq => write!(f, "=="),
            OpKind::NotEq => write!(f, "!="),
            OpKind::Comma => write!(f, ","),
        }
    }
}

impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LiteralValue::Bool(value) => write!(f, "{}", value),
            LiteralValue::Int32(value) => write!(f, "{}", value),
        }
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::BinOp(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::UnOp(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Identifier(ident) => write!(f, "{}", ident),
        }
    }
}
