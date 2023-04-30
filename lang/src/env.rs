use rpds::HashTrieMap;

use crate::ast::{BuiltInFunc, Ident, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    variables: HashTrieMap<Ident, Value>,
}

impl Environment {
    pub fn get(&self, identifier: &Ident) -> Option<&Value> {
        self.variables.get(identifier)
    }

    pub fn set(&self, identifier: Ident, value: Value) -> Self {
        Environment {
            variables: self.variables.insert(identifier, value),
        }
    }

    pub fn new() -> Self {
        Self {
            variables: HashTrieMap::new(),
        }
    }

    pub fn new_with_std() -> Self {
        use crate::parsing::parse;

        Self::new()
            .set(Ident::new("+"), Value::builtin_2(BuiltInFunc::Add))
            .set(Ident::new("-"), Value::builtin_2(BuiltInFunc::Sub))
            .set(Ident::new("*"), Value::builtin_2(BuiltInFunc::Mul))
            .set(Ident::new("/"), Value::builtin_2(BuiltInFunc::Div))
            .set(Ident::new("=="), Value::builtin_2(BuiltInFunc::Eq))
            .set(Ident::new(">"), Value::builtin_2(BuiltInFunc::Gt))
            .set(Ident::new(">="), Value::builtin_2(BuiltInFunc::Gte))
            .set(Ident::new("<"), Value::builtin_2(BuiltInFunc::Lt))
            .set(Ident::new("<="), Value::builtin_2(BuiltInFunc::Lte))
            .set(Ident::new("++"), Value::builtin_2(BuiltInFunc::Concat))
            .set(Ident::new("abs"), Value::builtin_1(BuiltInFunc::Abs))
            .set(
                Ident::new("|>"),
                Value::Function {
                    params: vec!["a".into(), "f".into()],
                    body: Box::new(parse("f a").unwrap()),
                    scope: Environment::new(),
                },
            )
    }
}
