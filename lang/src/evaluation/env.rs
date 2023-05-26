use rpds::HashTrieMap;
use crate::evaluation::builtin::BuiltInFunc;
use crate::evaluation::Environment;
use crate::evaluation::data::Value;
use crate::parsing::data::Ident;
use crate::parsing::parse_module;

#[derive(Debug, Clone, PartialEq, Default)]
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

    pub fn import_std(&self) -> Self {
        let stdlib = parse_module(
            r#"
            equalUpTo epsilon x y = abs (x - y) < epsilon
            max x y = if x > y then x else y
            min x y = if x < y then x else y
            (|>) a f = f a
            (>>) f g = x -> g (f x)
            (<<) f g = x -> f (g x)
        "#,
        )
            .expect("Could not parse Standard Library");

        self.set(Ident::new("+"), Value::builtin_2(BuiltInFunc::Add))
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
            .set(Ident::new("pow"), Value::builtin_2(BuiltInFunc::Pow))
            .eval(stdlib)
            .expect("Could not evaluate Standard Library")
            .1
    }
}