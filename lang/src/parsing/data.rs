
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub(crate) String);

impl Ident {
    pub fn new(name: &str) -> Self {
        Ident(name.to_string())
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Ident::new(name)
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
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

    pub fn iter(&self) -> impl Iterator<Item = &Ident> {
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

#[macro_export]
macro_rules! params {
    ($($param:ident),+) => {
        ParamsList::new(vec![$(Ident::new(stringify!($param))),*])
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Unit,
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
    Identifier(Ident),
    ConstantDefinition {
        name: Ident,
        expr: Box<Expr>,
    },
    FunctionApplication(Box<Expr>, Box<Expr>),
    FunctionDefinition {
        name: Ident,
        params: ParamsList,
        body: Box<Expr>,
    },
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

impl Expr {
    pub fn ident(name: &str) -> Expr {
        Expr::Identifier(Ident::new(name))
    }

    pub fn constdef(name: Ident, value: Expr) -> Expr {
        Expr::ConstantDefinition {
            name,
            expr: Box::new(value),
        }
    }

    pub fn fndef(name: Ident, params: ParamsList, body: Expr) -> Expr {
        Expr::FunctionDefinition {
            name,
            params,
            body: Box::new(body),
        }
    }

    pub fn fnapp(func: Expr, arg: Expr) -> Expr {
        Expr::FunctionApplication(Box::new(func), Box::new(arg))
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
pub struct Module {
    expressions: Vec<Expr>,
}

impl Module {
    pub fn new(expressions: Vec<Expr>) -> Self {
        Module { expressions }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Expr> {
        self.expressions.iter()
    }
}

impl IntoIterator for Module {
    type Item = Expr;
    type IntoIter = <Vec<Expr> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.expressions.into_iter()
    }
}

impl FromIterator<Expr> for Module {
    fn from_iter<T: IntoIterator<Item = Expr>>(iter: T) -> Self {
        Module::new(iter.into_iter().collect())
    }
}