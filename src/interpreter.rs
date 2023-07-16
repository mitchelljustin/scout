use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use anyhow::{anyhow, Error};

use Value::Nil;

use crate::ast::{Expr, Literal, Path, Program, Stmt};

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Error(String),
}

#[derive(Clone)]
pub struct UserFunction {
    name: String,
    params: Vec<String>,
    body: Expr,
}

pub type NativeFunction = fn(&mut Environment, Vec<Value>) -> anyhow::Result<Value>;

#[derive(Clone)]
pub enum Function {
    Native(NativeFunction),
    User(UserFunction),
}

pub struct Environment {
    modules: HashMap<String, Module>,
    open_module_name: String,
}

#[derive(Default)]
pub struct Module {
    name: String,
    items: HashMap<String, ModuleItem>,
}

pub enum ModuleItem {
    Function(Function),
    Variable(Value),
    Module(Module),
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

pub macro replace_expr($_t:tt $sub:expr) {
    $sub
}

pub macro count( $($xs:tt)* ) {
    0usize $(+ replace_expr!($xs 1usize))*
}

macro native_functions(
    [$const_name:ident]
    $(
        fn $fn_name:ident($env:ident, $args:ident) $body:tt
    )+
) {
    $(
        fn $fn_name($env: &mut Environment, $args: Vec<Value>) -> anyhow::Result<Value> $body
    )+
    pub const $const_name: [(&str, crate::interpreter::NativeFunction); crate::interpreter::count!($($fn_name)+)] = [
        $(
            (
                stringify!($fn_name),
                $fn_name,
            ),
        )+
    ];
}

mod builtin {
    use crate::interpreter::{native_functions, Value, Value::Nil};

    native_functions![
        [NATIVE_FUNCTIONS]
        fn print(_env, args) {
            let num_args = args.len();
            for (i, arg) in args.into_iter().enumerate() {
                print!("{arg}");
                if i < num_args - 1 {
                    print!(" ");
                }
            }
            println!();
            Ok(Nil)
        }
        fn add(_env, args) {
            if args.is_empty() {
                return Ok(Nil);
            }
            let result = args.into_iter().fold(Value::Number(0.0), |acc, arg| {
                if let (Value::Number(acc), Value::Number(val)) = (acc, arg) {
                    Value::Number(acc + val)
                } else {
                    Nil
                }
            });
            Ok(result)
        }
        fn concat(_env, args) {
            if args.is_empty() {
                return Ok(Nil);
            }
            let result = args.into_iter().fold(Value::String(String::new()), |acc, arg| {
                if let (Value::String(acc), Value::String(val)) = (acc, arg) {
                    Value::String(acc + &val)
                } else {
                    Nil
                }
            });
            Ok(result)
        }
    ];
}

const MODULE_MAIN: &str = "main";
const MODULE_STD: &str = "std";

impl Module {
    fn define_item(
        &mut self,
        item_type: &str,
        name: String,
        item: ModuleItem,
    ) -> Result<(), Error> {
        match self.items.try_insert(name.clone(), item) {
            Ok(_) => Ok(()),
            Err(_) => Err(anyhow!(
                "module '{}' already has {item_type} named '{name}'",
                self.name,
            )),
        }
    }

    pub fn define_native(&mut self, name: String, function: NativeFunction) -> anyhow::Result<()> {
        self.define_item(
            "native function",
            name,
            ModuleItem::Function(Function::Native(function)),
        )
    }

    pub fn define_function(&mut self, function: UserFunction) -> anyhow::Result<()> {
        self.define_item(
            "function",
            function.name.clone(),
            ModuleItem::Function(Function::User(function)),
        )
    }

    pub fn define_variable(&mut self, name: String, value: Value) -> anyhow::Result<()> {
        self.define_item("variable", name.clone(), ModuleItem::Variable(value))
    }
}

impl Environment {
    fn resolve(&self, path: Path) -> anyhow::Result<&ModuleItem> {
        let Ok([module_name, item_name]) = <[String; 2]>::try_from(path.0) else {
            return Err(anyhow!("only supports two-part paths for now"));
        };
        let Some(module) = self.modules.get(&module_name) else {
            return Err(anyhow!("no such module: '{module_name}'"));
        };
        let Some(item) = module.items.get(&item_name) else {
            return Err(anyhow!(
                "no such item in module '{module_name}': '{module_name}'"
            ));
        };
        Ok(item)
    }

    fn open_module(&mut self) -> &mut Module {
        self.modules.get_mut(&self.open_module_name).unwrap()
    }

    fn define_module(&mut self, name: String) -> anyhow::Result<&mut Module> {
        let error = anyhow!("module '{name}' already defined");
        self.modules
            .try_insert(name, Module::default())
            .map_err(|_| error)
    }

    pub fn new() -> Environment {
        let mut env = Environment {
            modules: Default::default(),
            open_module_name: MODULE_MAIN.to_string(),
        };
        env.define_module(MODULE_MAIN.to_string()).unwrap();
        let module_std = env.define_module(MODULE_STD.to_string()).unwrap();
        for (name, function) in builtin::NATIVE_FUNCTIONS {
            module_std
                .define_native(name.to_string(), function)
                .unwrap();
        }

        env
    }

    pub fn exec_source(&mut self, source: &str) -> anyhow::Result<()> {
        let Program { body } = source.parse()?;
        for stmt in body {
            self.exec(stmt)?;
        }
        Ok(())
    }

    fn exec(&mut self, stmt: Stmt) -> anyhow::Result<()> {
        match stmt {
            Stmt::ModuleDef { name, body } => {
                self.define_module(name.clone())?;
                self.open_module_name = name;
                for stmt in body {
                    self.exec(stmt)?;
                }
            }
            Stmt::FunctionDef { body, name, params } => {
                self.open_module()
                    .define_function(UserFunction { name, params, body })?;
            }
            Stmt::VariableDef { name, value } => {
                let value = self.eval(value)?;
                self.open_module().define_variable(name, value)?;
            }
            Stmt::Return { .. } => return Err(anyhow!("return outside of multiline expression")),
            Stmt::Expr { expr } => {
                self.eval(expr)?;
            }
        }
        Ok(())
    }

    fn eval(&mut self, expr: Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Multiline { body } => {
                let last_index = body.len() - 1;
                for (i, stmt) in body.into_iter().enumerate() {
                    match stmt {
                        Stmt::Return { retval: expr } => return self.eval(expr),
                        Stmt::Expr { expr } if i == last_index => return self.eval(expr),
                        stmt => {
                            self.exec(stmt)?;
                        }
                    }
                }
                Ok(Nil)
            }
            Expr::Variable { path } => {
                let error = anyhow!("not a variable: '{path}'");
                let ModuleItem::Variable(value) = self.resolve(path)? else {
                    return Err(error);
                };
                Ok(value.clone())
            }
            Expr::Literal { value } => Ok(match value {
                Literal::Nil => Nil,
                Literal::Bool(value) => Value::Bool(value),
                Literal::Number(value) => Value::Number(value),
                Literal::String(value) => Value::String(value),
            }),
            Expr::Call { path, args } => {
                let error = anyhow!("not a function: '{path}'");
                let ModuleItem::Function(function) = self.resolve(path)? else {
                    return Err(error);
                };
                let args = args
                    .into_iter()
                    .map(|arg| self.eval(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                match function {
                    Function::Native(native_function) => native_function(self, args),
                    Function::User(UserFunction { name, params, body }) => {}
                }
            }
        }
    }
}
