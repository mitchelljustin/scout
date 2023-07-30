use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use anyhow::{anyhow, Error};

use Value::Nil;

use crate::ast::{BinaryOp, Expr, Literal, Path, Program, Stmt};

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
    name: String,
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
    context: ScopeContext,
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

pub struct Environment {
    current_module_path: Path,
    root_module: Module,
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
        BinaryOp::Equal => "eq",
        BinaryOp::NotEqual => "ne",
    };
    Path(["std", "ops", fn_name].map(ToString::to_string).to_vec())
}

mod scout_std {
    use std::io::{stdin, stdout, BufRead, Write};

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
            stdout().lock().flush()?;
            Ok(Nil)
        }
        fn println(_env, args) {
            print(_env, args)?;
            println!();
            Ok(Nil)
        }
        fn readline(_env, args) {
            if !args.is_empty() {
                return Err(anyhow!("expected no arguments"));
            }
            let mut out = String::new();
            stdin().lock().read_line(&mut out)?;
            Ok(Value::String(out))
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
        fn num(_env, args) {
            expect_arity!([value] = args);
            let Value::String(string) = value else {
                return Err(anyhow!("expected string"));
            };
            let Ok(value) = string.trim().parse() else {
                return Ok(Nil);
            };
            Ok(Value::Number(value))
        }
        fn len(_env, args) {
            expect_arity!([arg] = args);
            Ok(Value::Number(match arg {
                Value::String(string) => string.len() as _,
                Value::Array(array) => array.len() as _,
                _ => return Err(anyhow!("expected string or array"))
            }))
        }
    ];

    pub mod ops {
        use anyhow::anyhow;

        use crate::interpreter::{native_functions, scout_std::expect_arity, Value};

        native_functions!(
            [NATIVE_FUNCTIONS]
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
            fn eq(_env, args) {
                expect_arity!([lhs, rhs] = args);
                Ok(Value::Bool(lhs == rhs))
            }
            fn ne(_env, args) {
                expect_arity!([lhs, rhs] = args);
                Ok(Value::Bool(lhs != rhs))
            }
        );
    }
}

const MODULE_ROOT: &str = "_main";
const MODULE_STD: &str = "std";
const MODULE_OPS: &str = "ops";

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
        let (module, item_name) = match &path.0.as_slice() {
            [item_name] => (self.current_module(), item_name),
            [module_path @ .., item_name] => {
                let Some(module) = self.find_module(module_path.iter()) else {
                    return Err(anyhow!(
                        "no such module: '{}'",
                        module_path.to_vec().join("::")
                    ));
                };
                (module, item_name)
            }
            _ => return Err(anyhow!("can only support `item` or `mod::item`")),
        };
        self.get_item_from_module(module, item_name)
    }

    fn get_item_from_module<'a>(
        &self,
        module: &'a Module,
        item_name: &str,
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
        let current_module_name = current_module.name.clone();
        current_module
            .items
            .try_insert(module_name.clone(), ModuleItem::Module(Module::default()))
            .map_err(|_| {
                anyhow!(
                    "module '{module_name}' already defined in '{}'",
                    current_module_name
                )
            })?;
        self.current_module_path.0.push(module_name.clone());
        Ok(())
    }

    fn end_module(&mut self) {
        self.current_module_path.0.pop();
    }

    pub fn new() -> Environment {
        let mut env = Environment {
            root_module: Module {
                items: Default::default(),
                name: MODULE_ROOT.to_string(),
            },
            scope_stack: Default::default(),
            current_module_path: Default::default(),
        };
        env.push_scope(ScopeContext::Root);
        env.init_stdlib();
        env
    }

    fn init_stdlib(&mut self) {
        self.define_module(MODULE_STD.to_string()).unwrap();
        self.define_native_functions(scout_std::NATIVE_FUNCTIONS);

        self.define_module(MODULE_OPS.to_string()).unwrap();
        self.define_native_functions(scout_std::ops::NATIVE_FUNCTIONS);
        self.end_module();

        self.end_module();
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
        self.eval_multiline(body, false)
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
                for item in target {
                    self.push_scope(ScopeContext::ForLoop);
                    self.define_variable(iterator.clone(), item)?;
                    self.eval_multiline(body.clone(), false)?;
                    self.pop_scope();
                }
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
            Stmt::Break => {
                unimplemented!();
                let Some((index, _)) =
                    self.scope_stack
                        .iter()
                        .enumerate()
                        .rev()
                        .find(|(_, scope)| {
                            matches!(
                                scope.context,
                                ScopeContext::ForLoop | ScopeContext::WhileLoop
                            )
                        })
                else {
                    return Err(anyhow!("'break' outside of for loop or while loop"));
                };
                self.scope_stack.truncate(index);
            }
            Stmt::Continue => unimplemented!(),
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
            Expr::If {
                condition,
                then_body,
                else_body,
            } => {
                let Value::Bool(condition) = self.eval(*condition)? else {
                    return Err(anyhow!("if condition expected to be a bool"));
                };
                if condition {
                    self.push_scope(ScopeContext::IfBody);
                    let result = self.eval_multiline(then_body, false)?;
                    self.pop_scope();
                    return Ok(result);
                }
                if !condition && let Some(else_body) = else_body {
                    self.push_scope(ScopeContext::ElseBody);
                    let result = self.eval_multiline(else_body, false)?;
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
        }
    }

    fn resolve_and_call(&mut self, path: &Path, args: Vec<Expr>) -> anyhow::Result<Value> {
        let args = args
            .into_iter()
            .map(|arg| self.eval(arg))
            .collect::<Result<Vec<_>, _>>()?;
        let ModuleItem::Function(function) = self.resolve_item(path)? else {
            return Err(anyhow!("not a function: {path}"));
        };
        match function.clone() {
            Function::Native(native_function) => native_function(self, args)
                .map_err(|err| anyhow!("while evaluating {path}(): {err}")),
            Function::User(UserFunction { params, body, .. }) => {
                if args.len() != params.len() {
                    return Err(anyhow!(
                        "arity mismatch, expected {}, got {}",
                        params.len(),
                        args.len()
                    ));
                }
                let func_id = format!("{path}({})", params.len());
                self.push_scope(ScopeContext::Function {
                    path: path.clone(),
                    arity: params.len(),
                });
                for (arg, param) in args.into_iter().zip(params) {
                    self.define_variable(param, arg)?;
                }
                let retval = self
                    .eval(body.clone())
                    .map_err(|err| anyhow!("while evaluating {func_id}: {err}"))?;
                self.pop_scope();
                Ok(retval)
            }
        }
    }

    fn eval_multiline(&mut self, body: Vec<Stmt>, is_function: bool) -> Result<Value, Error> {
        let last_index = body.len() - 1;
        for (i, stmt) in body.into_iter().enumerate() {
            match stmt {
                Stmt::Return { retval } if is_function => return self.eval(retval),
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
            context,
            variables: Default::default(),
        });
    }
}
