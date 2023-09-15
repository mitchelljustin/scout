use std::collections::HashMap;

use crate::interpreter::{error, NativeFunction};
use crate::interpreter::error::RuntimeError::AlreadyDefined;
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
    pub(crate) items: HashMap<String, Item>,
}

#[derive(Clone)]
pub enum Item {
    Function(Function),
    Module(Module),
}

impl Module {
    fn define_item(
        &mut self,
        item_type: &'static str,
        item_name: String,
        item: Item,
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
    
    pub fn define_native_function(&mut self, name: String, function: NativeFunction) -> error::Result<()> {
        self.define_item(
            "native function",
            name,
            Item::Function(Function::Native(function)),
        )
    }

    pub fn define_user_function(&mut self, function: UserFunction) -> error::Result<()> {
        self.define_item(
            "function",
            function.name.clone(),
            Item::Function(Function::User(function)),
        )
    }

    pub fn define_module(&mut self, name: &str) -> error::Result<()> {
        let module = Module {
            path: &self.path + name.to_string(),
            items: HashMap::default(),
        };
        self
            .items
            .try_insert(name.to_string(), Item::Module(module))
            .map_err(|_| AlreadyDefined {
                item_type: "module",
                item_name: name.to_string(),
                path: self.path.clone(),
            })?;
        Ok(())
    }
}
