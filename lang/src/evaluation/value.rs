use crate::{
    params,
    parsing::ast::{Expr, ParamsList},
};

use super::{builtin::BuiltInFunc, context::ValueEnvironment, error::EvalError};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit(),
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
    Function {
        params: ParamsList,
        body: Box<Expr>,
        scope: ValueEnvironment,
    },
    BuiltInFunction {
        name: BuiltInFunc,
        params: ParamsList,
        scope: ValueEnvironment,
    },
}

impl Value {
    pub fn builtin_1(name: BuiltInFunc) -> Value {
        Value::BuiltInFunction {
            name,
            params: params!(x),
            scope: ValueEnvironment::new(),
        }
    }

    pub fn builtin_2(name: BuiltInFunc) -> Value {
        Value::BuiltInFunction {
            name,
            params: params!(lhs, rhs),
            scope: ValueEnvironment::new(),
        }
    }
}

macro_rules! define_value_conversion {
    ($type:tt, $native_type:ty) => {
        impl TryFrom<Value> for $native_type {
            type Error = EvalError;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                match value {
                    Value::$type(value) => Ok(value),
                    _ => Err(EvalError::InvalidType {
                        value,
                        expected: stringify!($type),
                    }),
                }
            }
        }

        impl Into<Value> for $native_type {
            fn into(self) -> Value {
                Value::$type(self)
            }
        }
    };
}

define_value_conversion!(Int32, i32);
define_value_conversion!(Float32, f32);
define_value_conversion!(Bool, bool);
define_value_conversion!(String, String);
