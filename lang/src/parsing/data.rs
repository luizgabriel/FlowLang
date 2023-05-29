use std::ops::Index;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Name(String),
    Operator(String)
}

impl Ident {
    pub fn name(name: &str) -> Self {
        Ident::Name(name.to_string())
    }
    pub fn op(op: &str) -> Self {
        Ident::Operator(op.to_string())
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

    pub fn iter(&self) -> impl Iterator<Item=&Ident> {
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
    fn from_iter<T: IntoIterator<Item=Ident>>(iter: T) -> Self {
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
pub enum Declaration {
    Constant {
        name: Ident,
        expr: Box<Expr>,
    },
    Function {
        name: Ident,
        params: ParamsList,
        body: Box<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Bindings {
    bindings: Vec<Declaration>,
}

impl Bindings {
    pub fn new(bindings: Vec<Declaration>) -> Self {
        Bindings { bindings }
    }

    pub fn iter(&self) -> impl Iterator<Item=&Declaration> {
        self.bindings.iter()
    }

    pub fn len(&self) -> usize {
        self.bindings.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&Declaration> {
        self.bindings.get(index)
    }
}

impl Index<usize> for Bindings {
    type Output = Declaration;

    fn index(&self, index: usize) -> &Self::Output {
        &self.bindings[index]
    }
}

impl IntoIterator for Bindings {
    type Item = Declaration;
    type IntoIter = <Vec<Declaration> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.bindings.into_iter()
    }
}

impl FromIterator<Declaration> for Bindings {
    fn from_iter<T: IntoIterator<Item=Declaration>>(iter: T) -> Self {
        Bindings::new(iter.into_iter().collect())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Box<Expr>),
    Declaration(Box<Declaration>),
    LetBlock {
        bindings: Bindings,
        body: Box<Expr>,
    },
}

impl Declaration {
    pub fn constant(name: Ident, expr: Expr) -> Declaration {
        Declaration::Constant {
            name,
            expr: Box::new(expr),
        }
    }

    pub fn function(name: Ident, params: ParamsList, body: Statement) -> Declaration {
        Declaration::Function {
            name,
            params,
            body: Box::new(body),
        }
    }
}

impl Statement {
    pub fn expr(expr: Expr) -> Statement {
        Statement::Expression(Box::new(expr))
    }

    pub fn declaration(decl: Declaration) -> Statement {
        Statement::Declaration(Box::new(decl))
    }

    pub fn block(bindings: Bindings, body: Expr) -> Statement {
        Statement::LetBlock {
            bindings,
            body: Box::new(body),
        }
    }
}

impl Expr {
    pub fn ident_name(name: &str) -> Expr {
        Expr::Identifier(Ident::name(name))
    }

    pub fn ident_op(op: &str) -> Expr {
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

    pub fn iter(&self) -> impl Iterator<Item=&Statement> {
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
    fn from_iter<T: IntoIterator<Item=Statement>>(iter: T) -> Self {
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