use rpds::HashTrieMap;

use crate::parsing::{ast::Ident, parse_module};

use super::{builtin::BuiltInFunc, value::Value, Evaluator};

pub trait Environment: Sized + Clone {
    type Value;

    fn get(&self, identifier: &Ident) -> Option<&Self::Value>;
    fn set(&self, identifier: Ident, value: Self::Value) -> Self;

    fn pure<E>(&self, value: Self::Value) -> Result<(Self::Value, Self), E> {
        Ok((value, self.clone()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueEnvironment {
    variables: HashTrieMap<Ident, Value>,
}

impl Environment for ValueEnvironment {
    type Value = Value;

    fn get(&self, identifier: &Ident) -> Option<&Self::Value> {
        self.variables.get(identifier)
    }

    fn set(&self, identifier: Ident, value: Self::Value) -> Self {
        ValueEnvironment {
            variables: self.variables.insert(identifier, value),
        }
    }
}

impl ValueEnvironment {
    pub fn new() -> Self {
        Self {
            variables: HashTrieMap::new(),
        }
    }

    pub fn new_with_std() -> Self {
        let builtins = Self::new()
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
            .set(Ident::new("sqrt"), Value::builtin_1(BuiltInFunc::Sqrt))
            .set(Ident::new("pow"), Value::builtin_2(BuiltInFunc::Pow));

        let result = parse_module(
            "
                equalUpTo epsilon x y = abs (x - y) < epsilon

                (|>) a f = f a
                (>>) f g = x -> g (f x)
                (<<) f g = x -> f (g x)
                \0",
        )
        .map_err(|e| e.to_string())
        .and_then(|module| module.eval(builtins).map_err(|e| e.to_string()));

        match result {
            Ok((_, scope)) => scope,
            Err(e) => panic!("Failed to parse std module:\n{}", e),
        }
    }
}
