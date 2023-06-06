use crate::parsing::Ident;

pub trait Environment: Sized + Clone {
    type Value;

    fn get(&self, identifier: &Ident) -> Option<&Self::Value>;
    fn set(&self, identifier: Ident, value: Self::Value) -> Self;

    fn pure<E>(&self, value: Self::Value) -> Result<(Self::Value, Self), E> {
        Ok((value, self.clone()))
    }
}

pub mod evaluation;
pub mod parsing;
