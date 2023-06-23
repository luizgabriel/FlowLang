use crate::evaluation::builtin::BuiltInFunc;
use std::fmt::{Display, Formatter, Result};

use crate::evaluation::data::Value;

impl Display for BuiltInFunc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "#BuiltIn-{:?}", self)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Int32(value) => write!(f, "{}", value),
            Value::Float32(value) => write!(f, "{:.1}", value),
            Value::String(value) => write!(f, "\"{}\"", value),
            Value::Function {
                params,
                body,
                scope: _,
            } => write!(f, "({} -> {})", params, body),
            Value::BuiltInFunction {
                name,
                params,
                scope: _,
            } => {
                write!(f, "({} -> {})", params, name)
            }
        }
    }
}
