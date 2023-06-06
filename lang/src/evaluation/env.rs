use rpds::HashTrieMap;
use crate::Environment;

use crate::evaluation::builtin::BuiltInFunc;
use crate::evaluation::data::Value;
use crate::evaluation::Evaluator;
use crate::parsing::{parse_program, Ident, ParamsList};

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

macro_rules! builtin {
    // Receive Name and Args, *
    ($name:ident : $($param:ident),*) => {
        Value::BuiltInFunction {
            name: BuiltInFunc::$name,
            params: ParamsList::new(vec![$(Ident::name(stringify!($param))),*]),
            scope: ValueEnvironment::default(),
        }
    };
}

impl ValueEnvironment {
    pub fn import_std(&self) -> Self {
        let builtins = self
            .set(Ident::op("+"), builtin!(Add: lhs, rhs))
            .set(Ident::op("-"), builtin!(Sub: lhs, rhs))
            .set(Ident::op("*"), builtin!(Mul: lhs, rhs))
            .set(Ident::op("/"), builtin!(Div: lhs, rhs))
            .set(Ident::op("=="), builtin!(Eq: lhs, rhs))
            .set(Ident::op(">"), builtin!(Gt: lhs, rhs))
            .set(Ident::op(">="), builtin!(Gte: lhs, rhs))
            .set(Ident::op("<"), builtin!(Lt: lhs, rhs))
            .set(Ident::op("<="), builtin!(Lte: lhs, rhs))
            .set(Ident::op("++"), builtin!(Concat: lhs, rhs))
            .set(Ident::name("abs"), builtin!(Abs: x))
            .set(Ident::name("sqrt"), builtin!(Sqrt: x))
            .set(Ident::name("pow"), builtin!(Pow: x));

        parse_program(
            "
            equalUpTo epsilon x y = abs (x - y) < epsilon
            max x y = if x > y then x else y
            min x y = if x < y then x else y
            (|>) a f = f a
            (>>) f g = x -> g (f x)
            (<<) f g = x -> f (g x)
        ",
        )
        .expect("Could not parse Standard Library")
        .eval(builtins)
        .expect("Could not evaluate Standard Library")
        .1
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Ident, &Value)> {
        self.variables.iter()
    }
}
