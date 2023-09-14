use std::fmt::{Display, Formatter};

pub use error::RuntimeError;
pub use runtime::Runtime;

mod error;
mod module;
mod runtime;
mod stdlib;

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Primitive>),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Nil => write!(f, "nil"),
            Primitive::Bool(val) => write!(f, "{val}"),
            Primitive::Number(val) => write!(f, "{val}"),
            Primitive::String(val) => write!(f, "{val}"),
            Primitive::Array(vals) => {
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

pub type NativeFunction = fn(Vec<Primitive>) -> error::Result;
