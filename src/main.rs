#![feature(iter_next_chunk)]

use std::fs;

use crate::interpreter::Environment;

mod interpreter;

fn main() -> anyhow::Result<()> {
    let source = fs::read_to_string("./example.scout")?;
    let mut env = Environment::new();
    env.eval_source(&source)?;
    Ok(())
}
