use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use Value::Nil;

use crate::ast::{Expr, Program, Stmt};
use crate::interpreter::Function::Builtin;

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Error(String),
}

pub enum ModuleItem {
    Module { items: Vec<ModuleItem> },
    FunctionDef(FunctionDef),
    VariableDef { name: String, value: Value },
}

#[derive(Clone)]
struct FunctionDef {
    name: String,
    params: Vec<String>,
    body: Expr,
}

#[derive(Clone)]
enum Function {
    Builtin(fn(&mut Environment, Vec<Value>) -> Value),
    User(FunctionDef),
}

pub struct Environment {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Value::Bool(val) => write!(f, "{val}"),
            Value::Number(val) => write!(f, "{val}"),
            Value::String(val) => write!(f, "{val}"),
            Value::Error(val) => write!(f, "error({val})"),
        }
    }
}

macro builtins($(
    fn $fn_name:ident($env:ident, $args:ident) $body:tt
)+) {
    [
        $(
            (
                "std::".to_string() + stringify!($fn_name),
                Builtin(|$env, $args| $body),
            ),
        )+
    ]
}

impl Environment {
    fn initial_functions() -> HashMap<String, Function> {
        HashMap::from(builtins![
            fn print(_env, args) {
                let num_args = args.len();
                for (i, arg) in args.into_iter().enumerate() {
                    print!("{arg}");
                    if i < num_args - 1 {
                        print!(" ");
                    }
                }
                println!();
                Nil
            }
            fn add(_env, args) {
                if args.is_empty() {
                    return Nil;
                }
                args.into_iter().fold(Value::Number(0.0), |acc, arg| {
                    if let (Value::Number(acc), Value::Number(val)) = (acc, arg) {
                        Value::Number(acc + val)
                    } else {
                        Nil
                    }
                })
            }
            fn concat(_env, args) {
                if args.is_empty() {
                    return Nil;
                }
                args.into_iter().fold(Value::String(String::new()), |acc, arg| {
                    if let (Value::String(acc), Value::String(val)) = (acc, arg) {
                        Value::String(acc + &val)
                    } else {
                        Nil
                    }
                })
            }
        ])
    }

    pub fn new() -> Environment {
        Environment {
            variables: HashMap::new(),
            functions: Self::initial_functions(),
        }
    }

    pub fn exec_source(&mut self, source: &str) -> anyhow::Result<()> {
        let Program { body } = source.parse()?;
        for stmt in body {
            self.exec(stmt);
        }
        Ok(())
    }

    fn exec(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Module { .. } => {}
            Stmt::FunctionDef { .. } => {}
            Stmt::VariableDef { .. } => {}
        }
    }

    fn eval(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Multiline { body } => body.into_iter().fold(Nil, |_, expr| self.eval(expr)),
            Expr::Variable { path } => {}
            Expr::Literal { .. } => {}
            Expr::Call { .. } => {}
        }
    }

    fn do_call(&mut self, function: Function, args: Vec<Value>) -> Value {
        match function {
            Builtin(function) => function(self, args),
            Function::User(FunctionDef { params, body, .. }) => {
                if args.len() != params.len() {
                    return Nil;
                }
                for (param, arg) in params.iter().zip(args) {
                    self.variables.insert(param.clone(), arg); // todo: scoping
                }
                self.eval(body.clone())
            }
        }
    }
}
