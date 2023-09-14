use std::collections::{HashMap, VecDeque};
use std::ops::ControlFlow::{Break, Continue};
use std::ops::Deref;

use crate::interpreter::error::Result;
use crate::interpreter::error::RuntimeError::{
    AlreadyDefined, ArityMismatch, ControlFlowException, IllegalControlFlow, ItemNotFound,
    NoSuchModule, TypeMismatch, UndefinedVariable,
};
use crate::interpreter::module::{Function, Module, ModuleItem, UserFunction};
use crate::interpreter::Value::Nil;
use crate::interpreter::{stdlib, NativeFunction, Value};
use crate::parse::{Expr, Literal, Path, Program, Stmt};

#[derive(Default)]
pub struct Runtime {
    current_module_path: Path,
    root_module: Module,
    scope_stack: VecDeque<Scope>,
}

macro handle_control_flow($result:expr) {
    match $result {
        Err(ControlFlowException(Break(_))) => break,
        Err(ControlFlowException(Continue(_))) => continue,
        Err(error) => return Err(error),
        Ok(_) => {}
    }
}

impl Runtime {
    fn resolve_item(&self, path: &Path) -> Result<&ModuleItem> {
        let (module, item_name) = match path.0.as_slice() {
            [item_name] => (self.current_module(), item_name),
            [module_path @ .., item_name] => {
                let Some(module) = self.find_module(module_path.iter()) else {
                    return Err(NoSuchModule {
                        path: module_path.iter().map(Deref::deref).collect(),
                    });
                };
                (module, item_name)
            }
            pat => unreachable!("path pattern: {pat:?}"),
        };
        module.items.get(item_name).ok_or(ItemNotFound {
            path: module.path.clone(),
            item_name: item_name.clone(),
        })
    }

    fn resolve_var(&self, name: String) -> Result<&Value> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(&name) {
                return Ok(value);
            }
        }
        Err(UndefinedVariable { name })
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

    fn find_module_mut(&mut self, path: impl Iterator<Item = &String>) -> Option<&mut Module> {
        let mut module = &mut self.root_module;
        for step in path {
            let Some(ModuleItem::Module(sub_module)) = module.items.get_mut(step) else {
                return None;
            };
            module = sub_module;
        }
        Some(module)
    }

    fn current_module_mut(&mut self) -> &mut Module {
        self.find_module_mut(self.current_module_path.0.clone().iter())
            .unwrap()
    }

    pub(crate) fn define_module(&mut self, module_name: String) -> Result<()> {
        let current_module = self.current_module_mut();
        let module = Module {
            path: &current_module.path + module_name.clone(),
            items: Default::default(),
        };
        current_module
            .items
            .try_insert(module_name.clone(), ModuleItem::Module(module))
            .map_err(|_| AlreadyDefined {
                item_type: "module",
                item_name: module_name.clone(),
                path: current_module.path.clone(),
            })?;
        self.current_module_path.0.push(module_name);
        Ok(())
    }

    pub(crate) fn end_module(&mut self) {
        self.current_module_path.0.pop();
    }

    pub fn new() -> Runtime {
        let mut env = Runtime::default();
        env.push_scope(ScopeContext::Root);
        stdlib::init(&mut env);
        env
    }

    pub(crate) fn define_native_functions(
        &mut self,
        functions: impl IntoIterator<Item = (&str, NativeFunction)>,
    ) {
        for (name, function) in functions {
            self.current_module_mut()
                .define_native(name.to_string(), function)
                .unwrap();
        }
    }

    pub fn eval_source(&mut self, source: &str) -> Result<Value, anyhow::Error> {
        let Program { body } = source.parse()?;
        self.eval_multiline(body).map_err(From::from)
    }

    fn exec(&mut self, stmt: Stmt) -> Result<()> {
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
                    return Err(TypeMismatch {
                        expected: "array iterator",
                    });
                };
                for item in target {
                    self.push_scope(ScopeContext::ForLoop);
                    self.define_variable(iterator.clone(), item)?;
                    let result = self.eval_multiline(body.clone());
                    self.pop_scope();
                    handle_control_flow!(result);
                }
            }
            Stmt::WhileLoop { condition, body } => loop {
                loop {
                    let Value::Bool(condition) = self.eval(condition.clone())? else {
                        return Err(TypeMismatch {
                            expected: "boolean condition",
                        });
                    };
                    if !condition {
                        return Ok(());
                    }
                    self.push_scope(ScopeContext::WhileLoop);
                    let result = self.eval_multiline(body.clone());
                    self.pop_scope();
                    handle_control_flow!(result);
                }
            },
            Stmt::Return { .. } => {
                return Err(IllegalControlFlow {
                    keyword: "return",
                    expected_context: "function",
                })
            }
            Stmt::Break => {
                return Err(IllegalControlFlow {
                    keyword: "break",
                    expected_context: "loop",
                })
            }
            Stmt::Continue => {
                return Err(IllegalControlFlow {
                    keyword: "continue",
                    expected_context: "loop",
                })
            }
            Stmt::ClassDef { .. } => {
                unimplemented!()
            }
        }
        Ok(())
    }

    fn define_variable(&mut self, name: String, value: Value) -> Result<()> {
        if let Some(scope) = self
            .scope_stack
            .iter_mut()
            .rev()
            .find(|scope| scope.variables.contains_key(&name))
        {
            scope.variables.insert(name, value);
        } else {
            self.scope_stack
                .back_mut()
                .expect("no scope")
                .variables
                .insert(name, value);
        }
        Ok(())
    }

    fn eval(&mut self, expr: Expr) -> Result {
        match expr {
            Expr::Multiline { body } => self.eval_multiline(body),
            Expr::Variable { name } => {
                let value = self.resolve_var(name)?;
                Ok(value.clone())
            }
            Expr::If {
                condition,
                then_body,
                else_body,
            } => {
                let Value::Bool(condition) = self.eval(*condition)? else {
                    return Err(TypeMismatch {
                        expected: "boolean condition",
                    });
                };
                if condition {
                    self.push_scope(ScopeContext::IfBody);
                    let result = self.eval_multiline(then_body)?;
                    self.pop_scope();
                    return Ok(result);
                }
                if !condition && let Some(else_body) = else_body {
                    self.push_scope(ScopeContext::ElseBody);
                    let result = self.eval_multiline(else_body)?;
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
                Literal::Array(exprs) => Value::Array(self.eval_many(exprs)?),
            }),
            Expr::Binary { lhs, op, rhs } => {
                let path = stdlib::binary_op_to_fn_path(op);
                self.resolve_and_call(&path, vec![*lhs, *rhs])
            }
            Expr::Call { path, args } => self.resolve_and_call(&path, args),
        }
    }

    fn eval_many(&mut self, exprs: impl IntoIterator<Item = Expr>) -> Result<Vec<Value>> {
        exprs.into_iter().map(|arg| self.eval(arg)).collect()
    }

    fn resolve_and_call(&mut self, path: &Path, args: Vec<Expr>) -> Result {
        let args = self.eval_many(args)?;
        let ModuleItem::Function(function) = self.resolve_item(path).cloned()? else {
            return Err(TypeMismatch {
                expected: "module function",
            });
        };
        match function {
            Function::Native(native_function) => native_function(args),
            Function::User(UserFunction { params, body, .. }) => {
                if args.len() != params.len() {
                    return Err(ArityMismatch {
                        expected: params.len(),
                        actual: args.len(),
                    });
                }
                self.push_scope(ScopeContext::Function {
                    path: path.clone(),
                    arity: params.len(),
                });
                for (arg, param) in args.into_iter().zip(params) {
                    self.define_variable(param, arg)?;
                }
                let result = match body.clone() {
                    Expr::Multiline { body } => self.eval_multiline(body),
                    expr => self.eval(expr),
                };
                self.pop_scope();
                match result {
                    // `return`
                    Err(ControlFlowException(Break(retval))) | Ok(retval) => Ok(retval),
                    err => err,
                }
            }
        }
    }

    fn eval_multiline(&mut self, body: Vec<Stmt>) -> Result {
        if body.is_empty() {
            return Ok(Nil);
        }
        let last_index = body.len() - 1;
        for (i, stmt) in body.into_iter().enumerate() {
            match stmt {
                Stmt::Return { retval } => {
                    return Err(ControlFlowException(Break(self.eval(retval)?)))
                }
                Stmt::Continue => return Err(ControlFlowException(Continue(()))),
                Stmt::Break => return Err(ControlFlowException(Break(Nil))),
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
