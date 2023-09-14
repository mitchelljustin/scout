use std::collections::HashMap;

use crate::interpreter::error::RuntimeError::AlreadyDefined;
use crate::interpreter::{error, NativeFunction};
use crate::parse::{Expr, Path};

#[derive(Clone)]
pub struct UserFunction {
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) body: Expr,
}

#[derive(Clone)]
pub enum Function {
    Native(NativeFunction),
    User(UserFunction),
}

#[derive(Clone, Default)]
pub struct Module {
    pub(crate) path: Path,
    pub(crate) items: HashMap<String, ModuleItem>,
}

#[derive(Clone)]
pub struct Class {
    pub(crate) path: Path,
}

#[derive(Clone)]
pub enum ModuleItem {
    Function(Function),
    Module(Module),
    Class(Class),
}

impl Module {
    fn define_item(
        &mut self,
        item_type: &'static str,
        item_name: String,
        item: ModuleItem,
    ) -> error::Result<()> {
        match self.items.try_insert(item_name.clone(), item) {
            Ok(_) => Ok(()),
            Err(_) => Err(AlreadyDefined {
                path: self.path.clone(),
                item_type,
                item_name,
            }),
        }
    }

    pub fn define_native(&mut self, name: String, function: NativeFunction) -> error::Result<()> {
        self.define_item(
            "native function",
            name,
            ModuleItem::Function(Function::Native(function)),
        )
    }

    pub fn define_function(&mut self, function: UserFunction) -> error::Result<()> {
        self.define_item(
            "function",
            function.name.clone(),
            ModuleItem::Function(Function::User(function)),
        )
    }
}
