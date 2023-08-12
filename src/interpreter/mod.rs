use std::fmt::{Display, Formatter};
use std::ops::Deref;

pub use error::RuntimeError;
pub use runtime::Runtime;
use Value::Nil;

use crate::ast::{BinaryOp, Path};

mod error;
mod module;
mod runtime;
mod stdlib;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Value::Bool(val) => write!(f, "{val}"),
            Value::Number(val) => write!(f, "{val}"),
            Value::String(val) => write!(f, "{val}"),
            Value::Array(vals) => {
                write!(f, "[")?;
                for (i, val) in vals.iter().enumerate() {
                    write!(f, "{val}")?;
                    if i < vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

fn binary_op_to_fn_path(op: BinaryOp) -> Path {
    let op_fn_name = match op {
        BinaryOp::Add => "add",
        BinaryOp::Subtract => "sub",
        BinaryOp::Multiply => "mul",
        BinaryOp::Divide => "div",
        BinaryOp::Less => "lt",
        BinaryOp::Greater => "gt",
        BinaryOp::LessEqual => "lte",
        BinaryOp::GreaterEqual => "gte",
        BinaryOp::Equal => "eq",
        BinaryOp::NotEqual => "ne",
    };
    [stdlib::MODULE_STD, stdlib::MODULE_OPS, op_fn_name]
        .into_iter()
        .collect()
}

pub type NativeFunction = fn(Vec<Value>) -> error::Result;
