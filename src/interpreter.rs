use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

use Value::Nil;

use crate::interpreter::Function::Builtin;

#[derive(Parser)]
#[grammar = "scout.pest"]
struct ScoutParser;

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Value::Bool(val) => write!(f, "{val}"),
            Value::Number(val) => write!(f, "{val}"),
            Value::String(val) => write!(f, "{val}"),
        }
    }
}

fn pretty_print_pair(pair: Pair<Rule>, indent_level: usize) {
    println!(
        "{}{:?} '{}'",
        "| ".repeat(indent_level),
        pair.as_rule(),
        pair.as_str().replace('\n', "\\n")
    );
    for child in pair.into_inner() {
        pretty_print_pair(child, indent_level + 1);
    }
}

#[derive(Clone)]
enum Function<'a> {
    Builtin(fn(&mut Environment, Vec<Value>) -> Value),
    User {
        params: Vec<String>,
        body: Pair<'a, Rule>,
    },
}

pub struct Environment<'a> {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function<'a>>,
}

macro builtins($(
    fn $fn_name:ident($env:ident, $args:ident) $body:tt
)+) {
    [
        $(
            (
                "std::".to_string() + stringify!($fn_name),
                Builtin(|$env, $args| $body),
            ),
        )+
    ]
}

impl<'a> Environment<'a> {
    fn initial_functions() -> HashMap<String, Function<'a>> {
        HashMap::from(builtins![
            fn print(_env, args) {
                let num_args = args.len();
                for (i, arg) in args.into_iter().enumerate() {
                    print!("{arg}");
                    if i < num_args - 1 {
                        print!(" ");
                    }
                }
                println!();
                Nil
            }
            fn add(_env, args) {
                if args.is_empty() {
                    return Nil;
                }
                args.into_iter().fold(Value::Number(0.0), |acc, arg| {
                    if let (Value::Number(acc), Value::Number(val)) = (acc, arg) {
                        Value::Number(acc + val)
                    } else {
                        Nil
                    }
                })
            }
            fn concat(_env, args) {
                if args.is_empty() {
                    return Nil;
                }
                args.into_iter().fold(Value::String(String::new()), |acc, arg| {
                    if let (Value::String(acc), Value::String(val)) = (acc, arg) {
                        Value::String(acc + &val)
                    } else {
                        Nil
                    }
                })
            }
        ])
    }

    pub fn new() -> Environment<'a> {
        Environment {
            variables: HashMap::new(),
            functions: Self::initial_functions(),
        }
    }

    pub fn eval_source(&mut self, source: &'a str) -> anyhow::Result<()> {
        let program = ScoutParser::parse(Rule::program, source)?.next().unwrap();
        pretty_print_pair(program.clone(), 0);
        self.eval(program);
        Ok(())
    }

    fn eval(&mut self, pair: Pair<'a, Rule>) -> Value {
        match pair.as_rule() {
            Rule::program | Rule::multiline_expr => {
                pair.into_inner().fold(Nil, |_, expr| self.eval(expr))
            }
            Rule::expr => self.eval(pair.into_inner().next().unwrap()),
            Rule::func_def => {
                let [func_name, param_list, body] = pair.into_inner().next_chunk().unwrap();
                let func_name = func_name.as_str().to_string();
                let params = param_list
                    .into_inner()
                    .map(|p| p.as_str().to_string())
                    .collect();
                self.functions
                    .insert(func_name, Function::User { params, body });
                Nil
            }
            Rule::var_def => {
                let [target, value] = pair.into_inner().next_chunk().unwrap();
                let value = self.eval(value);
                self.variables
                    .insert(target.as_str().to_string(), value.clone());
                value
            }
            Rule::call => {
                let [func_name, arg_list] = pair.into_inner().next_chunk().unwrap();
                let func_name = func_name.as_str();
                let Some(function) = self.functions.get(func_name).cloned() else {
                    return Nil;
                };
                let args: Vec<Value> = arg_list.into_inner().map(|arg| self.eval(arg)).collect();
                self.do_call(function, args)
            }
            Rule::literal => self.eval(pair.into_inner().next().unwrap()),
            Rule::ident => self.variables.get(pair.as_str()).cloned().unwrap_or(Nil),
            Rule::number => match pair.as_str().parse::<f64>() {
                Ok(val) => Value::Number(val),
                Err(_) => Nil,
            },
            Rule::nil => Nil,
            Rule::bool => Value::Bool(match pair.as_str() {
                "true" => true,
                "false" => false,
                _ => unreachable!(),
            }),
            Rule::string => {
                Value::String(pair.into_inner().next().expect("what").as_str().to_string())
            }
            _ => Nil,
        }
    }

    fn do_call(&mut self, function: Function<'a>, args: Vec<Value>) -> Value {
        match function {
            Builtin(function) => function(self, args),
            Function::User { params, body } => {
                if args.len() != params.len() {
                    return Nil;
                }
                for (param, arg) in params.iter().zip(args) {
                    self.variables.insert(param.clone(), arg); // todo: scoping
                }
                self.eval(body.clone())
            }
        }
    }
}
