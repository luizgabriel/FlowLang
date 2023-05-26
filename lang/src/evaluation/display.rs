use std::fmt::{Display, Error, Formatter};
use crate::evaluation::data::Value;

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Int32(value) => write!(f, "{}", value),
            Value::Float32(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "\"{}\"", value),
            Value::Function {
                params,
                body,
                scope: _,
            } => write!(f, "({} -> {})", params, body,),
            Value::BuiltInFunction {
                name,
                params,
                scope: _,
            } => {
                write!(f, "({} -> {})", params, name,)
            }
        }
    }
}
