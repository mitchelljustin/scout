use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use anyhow::{anyhow, Error};

use Value::Nil;

use crate::ast::{BinaryOp, Expr, Literal, Path, Program, Stmt};

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Value>),
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

#[derive(Clone, Default)]
pub struct Module {
    name: String,
    items: HashMap<String, ModuleItem>,
}

#[derive(Clone)]
pub enum ModuleItem {
    Function(Function),
    // Variable(Value),
    // Module(Module),
}

pub struct Scope {
    _name: String,
    variables: HashMap<String, Value>,
}

pub struct Environment {
    modules: HashMap<String, Module>,
    current_module_name: String,
    scope_stack: VecDeque<Scope>,
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

fn binary_op_to_fn_path(op: BinaryOp) -> Path {
    let fn_name = match op {
        BinaryOp::Add => "add",
        BinaryOp::Subtract => "sub",
        BinaryOp::Multiply => "mul",
        BinaryOp::Divide => "div",
        BinaryOp::Less => "lt",
        BinaryOp::Greater => "gt",
        BinaryOp::LessEqual => "lte",
        BinaryOp::GreaterEqual => "gte",
    };
    Path(["std", fn_name].map(ToString::to_string).to_vec())
}

mod builtin {
    use anyhow::anyhow;

    use crate::interpreter::{count, native_functions, Value, Value::Nil};

    macro expect_arity([$($arg:ident),+] = $args:expr) {
        const ARITY: usize = count!($($arg)+);
        let Ok([$($arg),+]) = <[_; ARITY]>::try_from($args) else {
            return Err(anyhow!("expected {} arguments", ARITY));
        };
    }

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
            expect_arity!([lhs, rhs] = args);
            match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
                (Value::Array(mut lhs), Value::Array(rhs)) => {
                    lhs.extend(rhs);
                    Ok(Value::Array(lhs))
                }
                _ => Err(anyhow!("expected strings, numbers or arrays"))
            }
        }
        fn sub(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            Ok(Value::Number(lhs - rhs))
        }
        fn mul(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            Ok(Value::Number(lhs * rhs))
        }
        fn div(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            if rhs == 0.0 {
                return Err(anyhow!("cannot divide by zero"));
            }
            Ok(Value::Number(lhs / rhs))
        }
        fn str(_env, args) {
            Ok(
                Value::String(
                    args
                        .into_iter()
                        .fold(String::new(), |string, value| string + &value.to_string())
                )
            )
        }
        fn lt(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            Ok(Value::Bool(lhs < rhs))
        }
        fn gt(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            Ok(Value::Bool(lhs > rhs))
        }
        fn lte(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            Ok(Value::Bool(lhs <= rhs))
        }
        fn gte(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                return Err(anyhow!("expected numbers"));
            };
            Ok(Value::Bool(lhs >= rhs))
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
}

impl Environment {
    fn resolve_item(&self, path: &Path) -> anyhow::Result<&ModuleItem> {
        match &path.0.as_slice() {
            [item_name] => self.get_item_from_module(item_name, self.current_module()),
            [module_name, item_name] => {
                let Some(module) = self.modules.get(module_name) else {
                    return Err(anyhow!("no such module: '{module_name}'"));
                };
                self.get_item_from_module(item_name, module)
            }
            _ => Err(anyhow!("can only support `item` or `mod::item`")),
        }
    }

    fn get_item_from_module<'a>(
        &self,
        item_name: &str,
        module: &'a Module,
    ) -> anyhow::Result<&'a ModuleItem> {
        let Some(item) = module.items.get(item_name) else {
            return Err(anyhow!(
                "no such item in module '{}': '{item_name}'",
                module.name
            ));
        };
        Ok(item)
    }

    fn resolve_var(&self, name: &str) -> anyhow::Result<&Value> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return Ok(value);
            }
        }
        Err(anyhow!("no such variable: '{name}'"))
    }

    fn current_module(&self) -> &Module {
        self.modules.get(&self.current_module_name).unwrap()
    }

    fn current_module_mut(&mut self) -> &mut Module {
        self.modules.get_mut(&self.current_module_name).unwrap()
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
            current_module_name: MODULE_MAIN.to_string(),
            scope_stack: Default::default(),
        };
        env.push_scope("GLOBAL".to_string());
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
                if self.current_module_name != MODULE_MAIN {
                    return Err(anyhow!("can only define module at top level"));
                }
                self.define_module(name.clone())?;
                self.current_module_name = name;
                for stmt in body {
                    self.exec(stmt)?;
                }
            }
            Stmt::FunctionDef { body, name, params } => {
                self.current_module_mut()
                    .define_function(UserFunction { name, params, body })?;
            }
            Stmt::VariableDef { name, value } => {
                let value = self.eval(value)?;
                let Some(scope) = self.scope_stack.back_mut() else {
                    return Err(anyhow!("no scope"));
                };
                scope.variables.insert(name, value);
            }
            Stmt::Return { .. } => return Err(anyhow!("return outside of multiline expression")),
            Stmt::Expr { expr } => {
                self.eval(expr)?;
            }
            Stmt::ForLoop {
                iterator,
                target,
                body,
            } => {
                let Value::Array(target) = self.eval(target)? else {
                    return Err(anyhow!("for-in expected target to be an array"));
                };
                self.push_scope("for-in".to_string());
                for item in target {
                    self.define_variable(iterator.clone(), item)?;
                    self.eval_multiline(body.clone(), false)?;
                }
                self.pop_scope();
            }
            Stmt::WhileLoop { condition, body } => loop {
                loop {
                    let Value::Bool(condition) = self.eval(condition.clone())? else {
                        return Err(anyhow!("while expected boolean condition"));
                    };
                    if !condition {
                        return Ok(());
                    }
                    self.eval_multiline(body.clone(), false)?;
                }
            },
        }
        Ok(())
    }

    fn define_variable(&mut self, name: String, value: Value) -> anyhow::Result<()> {
        let Some(scope) = self.scope_stack.back_mut() else {
            return Err(anyhow!("variable defined outside of a scope"));
        };
        scope.variables.insert(name, value);
        Ok(())
    }

    fn eval(&mut self, expr: Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Multiline { body } => self.eval_multiline(body, true),
            Expr::Variable { name } => {
                let value = self.resolve_var(&name)?;
                Ok(value.clone())
            }
            Expr::Literal { value } => Ok(match value {
                Literal::Nil => Nil,
                Literal::Bool(value) => Value::Bool(value),
                Literal::Number(value) => Value::Number(value),
                Literal::String(value) => Value::String(value),
                Literal::Array(exprs) => Value::Array(
                    exprs
                        .into_iter()
                        .map(|expr| self.eval(expr))
                        .collect::<Result<_, _>>()?,
                ),
            }),
            Expr::Binary { lhs, op, rhs } => {
                let path = binary_op_to_fn_path(op);
                self.resolve_and_call(&path, vec![*lhs, *rhs])
            }
            Expr::Call { path, args } => self.resolve_and_call(&path, args),
            _ => Ok(Nil),
        }
    }

    fn resolve_and_call(&mut self, path: &Path, args: Vec<Expr>) -> anyhow::Result<Value> {
        let args = args
            .into_iter()
            .map(|arg| self.eval(arg))
            .collect::<Result<Vec<_>, _>>()?;
        let ModuleItem::Function(function) = self.resolve_item(path)?;
        match function.clone() {
            Function::Native(native_function) => native_function(self, args),
            Function::User(UserFunction { name, params, body }) => {
                if args.len() != params.len() {
                    return Err(anyhow!(
                        "arity mismatch, expected {}, got {}",
                        params.len(),
                        args.len()
                    ));
                }
                self.push_scope(format!("fn {name}({})", params.len()));
                for (arg, param) in args.into_iter().zip(params) {
                    self.define_variable(param.clone(), arg)?;
                }
                let retval = self.eval(body.clone())?;
                self.pop_scope();
                Ok(retval)
            }
        }
    }

    fn eval_multiline(&mut self, body: Vec<Stmt>, allow_return: bool) -> Result<Value, Error> {
        let last_index = body.len() - 1;
        for (i, stmt) in body.into_iter().enumerate() {
            match stmt {
                Stmt::Return { retval } if allow_return => return self.eval(retval),
                Stmt::Expr { expr } if i == last_index => return self.eval(expr),
                stmt => {
                    self.exec(stmt)?;
                }
            }
        }
        Ok(Nil)
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop_back();
    }

    fn push_scope(&mut self, name: String) {
        self.scope_stack.push_back(Scope {
            _name: name,
            variables: Default::default(),
        });
    }
}
