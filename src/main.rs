#![feature(iter_next_chunk)]
#![feature(decl_macro)]
#![feature(map_try_insert)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(let_chains)]

extern crate core;

use std::io::{BufRead, Write};
use std::{fs, io};

use interpreter::Runtime;

use crate::interpreter::Value;

mod ast;
mod interpreter;

fn main() -> anyhow::Result<()> {
    let mut env = Runtime::new();
    let source = fs::read_to_string("./lib.scout")?;
    env.eval_source(&source)?;
    print!(">> ");
    io::stdout().flush()?;
    for line in io::stdin().lock().lines() {
        let line = line? + "\n";
        match env.eval_source(&line) {
            Err(error) => println!("!! {error}\n"),
            Ok(Value::Nil) => {}
            Ok(result) => println!("{result}"),
        }
        print!(">> ");
        io::stdout().flush()?;
    }
    Ok(())
}
