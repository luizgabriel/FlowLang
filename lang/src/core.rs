use crate::parsing::Ident;

pub trait Environment: Sized + Clone {
    type Value;

    fn get(&self, identifier: &Ident) -> Option<&Self::Value>;
    fn set(&self, identifier: Ident, value: Self::Value) -> Self;

    fn eval<T, E>(&self, evaluator: T) -> Result<(Self::Value, Self), E>
    where
        T: Evaluator<Context = Self, Output = Self::Value, Error = E>,
    {
        let (value, next_env) = evaluator.eval(self.clone())?;
        Ok((value, next_env))
    }

    fn pure<E>(&self, value: Self::Value) -> Result<(Self::Value, Self), E> {
        Ok((value, self.clone()))
    }
}

pub trait Evaluator {
    type Output;
    type Context;
    type Error;

    fn eval(&self, context: Self::Context) -> Result<(Self::Output, Self::Context), Self::Error>;
}
