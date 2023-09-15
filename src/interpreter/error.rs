use std::ops::ControlFlow;

use thiserror::Error;

use crate::interpreter::Primitive;
use crate::parse::Path;

#[allow(clippy::module_name_repetitions)]
#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("arity mismatch: {function})() expected {expected} args, got {actual} args")]
    ArityMismatch {
        function: String,
        expected: usize,
        actual: usize,
    },

    #[error("type mismatch: expected {expected}")]
    TypeMismatch { expected: &'static str },

    #[error("cannot divide by zero")]
    DivideByZero,

    #[error("{item_type} '{item_name}' already defined in {path}'")]
    AlreadyDefined {
        path: Path,
        item_name: String,
        item_type: &'static str,
    },

    #[error("no such module: {path}")]
    NoSuchModule { path: Path },

    #[error("undefined variable: '{name}'")]
    UndefinedVariable { name: String },

    #[error("item '{item_name}' not found in {path}")]
    ItemNotFound { path: Path, item_name: String },

    #[error("illegal `{keyword}` outside of {expected_context}")]
    IllegalControlFlow {
        keyword: &'static str,
        expected_context: &'static str,
    },

    #[error("control flow")]
    ControlFlowException(ControlFlow<Primitive>),
}

pub type Result<T = Primitive, E = RuntimeError> = std::result::Result<T, E>;
