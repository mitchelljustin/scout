use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use anyhow::{anyhow, bail, Context, Error};

use Value::Nil;

use crate::ast::{BinaryOp, Expr, Literal, Path, Program, Stmt};
use crate::interpreter::stdlib::{MODULE_OPS, MODULE_STD};

mod stdlib;

#[derive(Clone, Debug, PartialEq)]
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
    path: Path,
    items: HashMap<String, ModuleItem>,
}

#[derive(Clone)]
pub enum ModuleItem {
    Function(Function),
    Module(Module),
    // Variable(Value),
}

#[derive(Debug)]
pub struct Scope {
    _context: ScopeContext,
    variables: HashMap<String, Value>,
}

#[derive(Debug)]
pub enum ScopeContext {
    Root,
    Function { path: Path, arity: usize },
    WhileLoop,
    ForLoop,
    IfBody,
    ElseBody,
}

#[derive(Default)]
pub struct Environment {
    current_module_path: Path,
    root_module: Module,
    scope_stack: VecDeque<Scope>,
    is_breaking_loop: bool,
}

trait IterExt<T>: IntoIterator<Item = T> {
    fn try_map_collect<B, E, F, Col>(self, f: F) -> Result<Col, E>
    where
        F: FnMut(T) -> Result<B, E>,
        Col: FromIterator<B>;
}

impl<I, T> IterExt<T> for I
where
    I: IntoIterator<Item = T>,
{
    fn try_map_collect<B, E, F, Col>(self, f: F) -> Result<Col, E>
    where
        F: FnMut(T) -> Result<B, E>,
        Col: FromIterator<B>,
    {
        self.into_iter().map(f).collect()
    }
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
    [MODULE_STD, MODULE_OPS, op_fn_name].into()
}

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
                self.path,
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
        let (module, item_name) = match path.0.as_slice() {
            [item_name] => (self.current_module(), item_name),
            [module_path @ .., item_name] => {
                let Some(module) = self.find_module(module_path.iter()) else {
                    bail!("no such module: '{}'", module_path.to_vec().join("::"));
                };
                (module, item_name)
            }
            pat => unreachable!("path pattern: {pat:?}"),
        };
        module.items.get(item_name).ok_or(anyhow!(
            "no such item in module '{}': '{item_name}'",
            module.path
        ))
    }

    fn resolve_var(&self, name: &str) -> anyhow::Result<&Value> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return Ok(value);
            }
        }
        bail!("undefined variable: '{name}'")
    }

    fn current_module(&self) -> &Module {
        self.find_module(self.current_module_path.0.iter()).unwrap()
    }

    fn find_module(&self, path: impl Iterator<Item = &String>) -> Option<&Module> {
        let mut module = &self.root_module;
        for step in path {
            let Some(ModuleItem::Module(sub_module)) = module.items.get(step) else {
                return None;
            };
            module = sub_module;
        }
        Some(module)
    }

    fn current_module_mut(&mut self) -> &mut Module {
        let mut module = &mut self.root_module;
        for step in &self.current_module_path.0 {
            let Some(ModuleItem::Module(sub_module)) = module.items.get_mut(step) else {
                panic!("current module not found");
            };
            module = sub_module;
        }
        module
    }

    fn define_module(&mut self, module_name: String) -> anyhow::Result<()> {
        let current_module = self.current_module_mut();
        let module = Module {
            path: &current_module.path + module_name.clone(),
            items: Default::default(),
        };
        current_module
            .items
            .try_insert(module_name.clone(), ModuleItem::Module(module))
            .map_err(|_| {
                anyhow!(
                    "module '{module_name}' already defined in '{}'",
                    current_module.path
                )
            })?;
        self.current_module_path.0.push(module_name);
        Ok(())
    }

    fn end_module(&mut self) {
        self.current_module_path.0.pop();
    }

    pub fn new() -> Environment {
        let mut env = Environment::default();
        env.push_scope(ScopeContext::Root);
        stdlib::init(&mut env);
        env
    }

    fn define_native_functions<const N: usize>(&mut self, functions: [(&str, NativeFunction); N]) {
        for (name, function) in functions {
            self.current_module_mut()
                .define_native(name.to_string(), function)
                .unwrap();
        }
    }

    pub fn eval_source(&mut self, source: &str) -> anyhow::Result<Value> {
        let Program { body } = source.parse()?;
        self.eval_multiline(body, false, false)
    }

    fn exec(&mut self, stmt: Stmt) -> anyhow::Result<()> {
        match stmt {
            Stmt::ModuleDef { name, body } => {
                self.define_module(name.clone())?;
                for stmt in body {
                    self.exec(stmt)?;
                }
                self.end_module();
            }
            Stmt::FunctionDef { body, name, params } => {
                self.current_module_mut()
                    .define_function(UserFunction { name, params, body })?;
            }
            Stmt::VariableDef { name, value } => {
                let value = self.eval(value)?;
                self.define_variable(name, value)?;
            }
            Stmt::Expr { expr } => {
                self.eval(expr)?;
            }
            Stmt::ForLoop {
                iterator,
                target,
                body,
            } => {
                let Value::Array(target) = self.eval(target)? else {
                    bail!("`for _ in _` expected target to be an array");
                };
                for item in target {
                    self.push_scope(ScopeContext::ForLoop);
                    self.define_variable(iterator.clone(), item)?;
                    self.eval_multiline(body.clone(), false, false)?;
                    self.pop_scope();
                    if self.is_breaking_loop {
                        self.is_breaking_loop = false;
                        break;
                    }
                }
            }
            Stmt::WhileLoop { condition, body } => loop {
                loop {
                    let Value::Bool(condition) = self.eval(condition.clone())? else {
                        bail!("`while` expected boolean condition");
                    };
                    if !condition {
                        return Ok(());
                    }
                    self.push_scope(ScopeContext::WhileLoop);
                    self.eval_multiline(body.clone(), false, false)?;
                    self.pop_scope();
                    if self.is_breaking_loop {
                        self.is_breaking_loop = false;
                        break;
                    }
                }
            },
            Stmt::Return { .. } => bail!("`return` outside of function"),
            Stmt::Break => bail!("`break` outside of loop"),
            Stmt::Continue => bail!("`continue` outside of loop"),
        }
        Ok(())
    }

    fn define_variable(&mut self, name: String, value: Value) -> anyhow::Result<()> {
        let Some(scope) = self.scope_stack.back_mut() else {
            panic!("no scope");
        };
        scope.variables.insert(name, value);
        Ok(())
    }

    fn eval(&mut self, expr: Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Multiline { body } => self.eval_multiline(body, false, false),
            Expr::Variable { name } => {
                let value = self.resolve_var(&name)?;
                Ok(value.clone())
            }
            Expr::If {
                condition,
                then_body,
                else_body,
            } => {
                let Value::Bool(condition) = self.eval(*condition)? else {
                    bail!("if condition expected to be a bool");
                };
                if condition {
                    self.push_scope(ScopeContext::IfBody);
                    let result = self.eval_multiline(then_body, false, false)?;
                    self.pop_scope();
                    return Ok(result);
                }
                if !condition && let Some(else_body) = else_body {
                    self.push_scope(ScopeContext::ElseBody);
                    let result = self.eval_multiline(else_body, false, false)?;
                    self.pop_scope();
                    return Ok(result);
                }
                Ok(Nil)
            }
            Expr::Literal { value } => Ok(match value {
                Literal::Nil => Nil,
                Literal::Bool(value) => Value::Bool(value),
                Literal::Number(value) => Value::Number(value),
                Literal::String(value) => Value::String(value),
                Literal::Array(exprs) => Value::Array(exprs.try_map_collect(|arg| self.eval(arg))?),
            }),
            Expr::Binary { lhs, op, rhs } => {
                let path = binary_op_to_fn_path(op);
                self.resolve_and_call(&path, vec![*lhs, *rhs])
            }
            Expr::Call { path, args } => self.resolve_and_call(&path, args),
        }
    }

    fn resolve_and_call(&mut self, path: &Path, args: Vec<Expr>) -> anyhow::Result<Value> {
        let args = args.try_map_collect(|arg| self.eval(arg))?;
        let ModuleItem::Function(function) = self.resolve_item(path)? else {
            bail!("not a function: {path}");
        };
        match function.clone() {
            Function::Native(native_function) => {
                native_function(self, args).context(format!("while evaluating {path}()"))
            }
            Function::User(UserFunction { params, body, .. }) => {
                let context = format!("while evaluating {path}({})", params.len());

                if args.len() != params.len() {
                    bail!(
                        "{context}: arity mismatch, expected {}, got {}",
                        params.len(),
                        args.len()
                    );
                }
                self.push_scope(ScopeContext::Function {
                    path: path.clone(),
                    arity: params.len(),
                });
                for (arg, param) in args.into_iter().zip(params) {
                    self.define_variable(param, arg)?;
                }
                let retval = match body.clone() {
                    Expr::Multiline { body } => self.eval_multiline(body, true, false),
                    expr => self.eval(expr),
                }?;
                self.pop_scope();
                Ok(retval)
            }
        }
    }

    fn eval_multiline(
        &mut self,
        body: Vec<Stmt>,
        is_function: bool,
        is_loop: bool,
    ) -> Result<Value, Error> {
        if body.is_empty() {
            return Ok(Nil);
        }
        let last_index = body.len() - 1;
        for (i, stmt) in body.into_iter().enumerate() {
            match stmt {
                Stmt::Return { retval } if is_function => return self.eval(retval),
                Stmt::Continue if is_loop => break,
                Stmt::Break if is_loop => {
                    self.is_breaking_loop = true;
                    break;
                }
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

    fn push_scope(&mut self, context: ScopeContext) {
        self.scope_stack.push_back(Scope {
            _context: context,
            variables: Default::default(),
        });
    }
}
