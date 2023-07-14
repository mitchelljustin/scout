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

#[derive(Clone)]
enum Function<'a> {
    Builtin(fn(&mut Environment, &[Value]) -> Value),
    User {
        params: Vec<String>,
        body: Pair<'a, Rule>,
    },
}

pub struct Environment<'a> {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function<'a>>,
}

impl<'a> Environment<'a> {
    fn init_functions() -> HashMap<String, Function<'a>> {
        HashMap::from([(
            "print".to_string(),
            Builtin(|_env, args: &[Value]| {
                for (i, arg) in args.iter().enumerate() {
                    print!("{arg}");
                    if i < args.len() - 1 {
                        print!(" ");
                    }
                }
                println!();
                Nil
            }),
        )])
    }

    pub fn new() -> Environment<'a> {
        Environment {
            variables: HashMap::new(),
            functions: Self::init_functions(),
        }
    }

    pub fn eval_source(&mut self, source: &str) -> anyhow::Result<()> {
        let program = ScoutParser::parse(Rule::program, source)?.next().unwrap();
        self.eval(program);
        Ok(())
    }

    fn eval(&mut self, pair: Pair<Rule>) -> Value {
        match pair.as_rule() {
            Rule::program => pair.into_inner().fold(Nil, |_, expr| self.eval(expr)),
            Rule::expr => self.eval(pair.into_inner().next().unwrap()),
            Rule::func_def => {
                let [call, expr] = pair.into_inner().next_chunk().unwrap();
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
            Rule::string => Value::String(pair.into_inner().next().unwrap().as_str().to_string()),
            _ => Nil,
        }
    }

    fn do_call(&mut self, function: Function, args: Vec<Value>) -> Value {
        match function {
            Builtin(function) => function(self, &args),
            Function::User { params, body } => {
                if args.len() != params.len() {
                    return Nil;
                }
                for (param, arg) in params.iter().zip(args.into_iter()) {
                    self.variables.insert(param.clone(), arg);
                }
                self.eval(body.clone())
            }
        }
    }
}
