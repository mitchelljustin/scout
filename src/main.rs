#![feature(iter_next_chunk)]
#![feature(decl_macro)]
#![feature(map_try_insert)]
#![feature(anonymous_lifetime_in_impl_trait)]

extern crate core;

use std::fs;

use crate::interpreter::Environment;

mod ast;
mod interpreter;

fn main() -> anyhow::Result<()> {
    let source = fs::read_to_string("./example.scout")?;
    let mut env = Environment::new();
    env.exec_source(&source)?;
    Ok(())
}
