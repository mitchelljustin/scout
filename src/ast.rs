use anyhow::anyhow;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::str::FromStr;

#[derive(Parser)]
#[grammar = "scout.pest"]
struct ScoutParser;

#[derive(Debug, Clone)]
pub struct Program {
    body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Path(Vec<String>);

#[derive(Debug, Clone)]
pub enum Expr {
    Multiline { body: Vec<Stmt> },
    Variable { path: Path },
    Literal { value: Literal },
    Call { target: Path, args: Vec<Expr> },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Module {
        name: String,
        body: Vec<Stmt>,
    },
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
    VariableDef {
        name: String,
        value: Expr,
    },
    Return {
        retval: Expr,
    },
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

impl FromStr for Program {
    type Err = anyhow::Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let pair = ScoutParser::parse(Rule::program, source)?.next().unwrap();
        pretty_print_pair(pair, 0);
        pair.try_into()
    }
}

impl TryFrom<Pair<'_, Rule>> for Program {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<Rule>) -> Result<Self, Self::Error> {
        let Rule::program = pair.as_rule() else {
            return Err(anyhow!("expected program"));
        };
        let body = pair
            .into_inner()
            .map(Stmt::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Program { body })
    }
}

impl TryFrom<Pair<'_, Rule>> for Stmt {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::func_def => {
                let [name, params, body] = pair.into_inner().next_chunk().unwrap();
                let name = name.as_str().to_string();
                let params = params
                    .into_inner()
                    .map(|param| param.as_str().to_string())
                    .collect();
                let body = body.try_into()?;

                Ok(Stmt::FunctionDef { name, params, body })
            }
            Rule::var_def => {
                let [name, value] = pair.into_inner().next_chunk().unwrap();
                let name = name.as_str().to_string();
                let value = value.try_into()?;
                Ok(Stmt::VariableDef { name, value })
            }

            _ => Err(anyhow!("expected statement pair")),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for Expr {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            _ => Err(anyhow!("expected expression pair")),
        }
    }
}
