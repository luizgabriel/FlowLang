use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Name(String),
    Operator(String),
}

impl Ident {
    pub fn name(name: &str) -> Self {
        Ident::Name(name.to_string())
    }

    pub fn op(op: &str) -> Self {
        Ident::Operator(op.to_string())
    }

    pub fn as_str(&self) -> &str {
        match self {
            Ident::Name(name) => name,
            Ident::Operator(op) => op,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamsList {
    params: Vec<Ident>,
}

impl ParamsList {
    pub fn new(params: Vec<Ident>) -> Self {
        ParamsList { params }
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn get(&self, index: usize) -> Option<&Ident> {
        self.params.get(index)
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Ident> {
        self.params.iter()
    }

    pub fn split_first(&self) -> Option<(Ident, ParamsList)> {
        let (head, tail) = self.params.split_first()?;
        Some((head.clone(), Self::new(tail.to_vec())))
    }
}

impl IntoIterator for ParamsList {
    type Item = Ident;
    type IntoIter = <Vec<Ident> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.params.into_iter()
    }
}

impl FromIterator<Ident> for ParamsList {
    fn from_iter<T: IntoIterator<Item = Ident>>(iter: T) -> Self {
        ParamsList::new(iter.into_iter().collect())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Unit,
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
    Identifier(Ident),
    FunctionApplication(Box<Expr>, Box<Expr>),
    Lambda {
        params: ParamsList,
        body: Box<Expr>,
    },
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleName(Vec<Ident>);

impl ModuleName {
    pub fn new(names: Vec<Ident>) -> Self {
        ModuleName(names)
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Ident> {
        self.0.iter()
    }

    pub fn as_path(&self) -> PathBuf {
        self.iter()
            .map(|ident| ident.to_string())
            .collect::<PathBuf>()
            .with_extension("fw")
    }
}

impl IntoIterator for ModuleName {
    type Item = Ident;
    type IntoIter = <Vec<Ident> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    UseModule(ModuleName),
    Expression(Expr),
    ConstantDeclaration {
        name: Ident,
        expr: Expr,
    },
    FunctionDeclaration {
        name: Ident,
        params: ParamsList,
        body: Box<Statement>,
    },
    Block(Vec<Statement>),
}

impl Statement {
    pub fn expr(expr: Expr) -> Self {
        Statement::Expression(expr)
    }

    pub fn constant(name: Ident, expr: Expr) -> Self {
        Statement::ConstantDeclaration { name, expr }
    }

    pub fn function(name: Ident, params: ParamsList, body: Statement) -> Self {
        Statement::FunctionDeclaration {
            name,
            params,
            body: Box::new(body),
        }
    }
}

impl Expr {
    pub fn name(name: &str) -> Expr {
        Expr::Identifier(Ident::name(name))
    }

    pub fn operator(op: &str) -> Expr {
        Expr::Identifier(Ident::op(op))
    }

    pub fn fnapp(func: Expr, arg: Expr) -> Expr {
        Expr::FunctionApplication(Box::new(func), Box::new(arg))
    }

    pub fn fnapp2(func: Expr, lhs: Expr, rhs: Expr) -> Expr {
        Self::fnapp(Self::fnapp(func, lhs), rhs)
    }

    pub fn lambda(params: ParamsList, body: Expr) -> Expr {
        Expr::Lambda {
            params,
            body: Box::new(body),
        }
    }

    pub fn ife(condition: Expr, then: Expr, otherwise: Expr) -> Expr {
        Expr::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }
}

impl IntoIterator for Program {
    type Item = Statement;
    type IntoIter = <Vec<Statement> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

impl FromIterator<Statement> for Program {
    fn from_iter<T: IntoIterator<Item = Statement>>(iter: T) -> Self {
        Program::new(iter.into_iter().collect())
    }
}

macro_rules! impl_from_for_expr {
    ($ty:ty, $name:ident) => {
        impl From<$ty> for Expr {
            fn from(value: $ty) -> Self {
                Expr::$name(value)
            }
        }
    };
}

impl_from_for_expr!(i32, Int32);
impl_from_for_expr!(f32, Float32);
impl_from_for_expr!(bool, Bool);
impl_from_for_expr!(String, String);

impl From<Expr> for Statement {
    fn from(expr: Expr) -> Self {
        Statement::expr(expr)
    }
}

impl From<Box<Expr>> for Box<Statement> {
    fn from(expr: Box<Expr>) -> Self {
        Box::new(Statement::expr(*expr))
    }
}
