use crate::ast::Path;
use crate::interpreter::Value;

pub enum Instruction {
    LocalGet { name: String },
    LocalSet { name: String },
    Call { path: Path },
    Push { value: Value },
    DefineModule { name: String },
    EndModule { name: String },
    DefineFunction { name: String, params: Vec<String> },
    EndFunction { name: String },
    Return,

    End { id: usize },
    If { id: usize },
    Else { id: usize },
    ForIn { iterator: String, id: usize },
    While { id: usize },

    Continue { id: usize },
    Break { id: usize },
}
