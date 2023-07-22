use std::fmt::{Display, Formatter};
use std::str::FromStr;

use anyhow::anyhow;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "scout.pest"]
struct ScoutParser;

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Path(pub(crate) Vec<String>);

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone().join("::"))
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Multiline { body: Vec<Stmt> },
    Variable { name: String },
    Literal { value: Literal },
    Call { path: Path, args: Vec<Expr> },
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
    ModuleDef {
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
    Expr {
        expr: Expr,
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
        pretty_print_pair(pair.clone(), 0);
        pair.try_into()
    }
}

impl TryFrom<Pair<'_, Rule>> for Program {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<Rule>) -> Result<Self, Self::Error> {
        let Rule::program = pair.as_rule() else {
            return Err(anyhow!("expected program"));
        };
        let inner = pair.into_inner().next().unwrap();
        let body = inner
            .into_inner()
            .map(Stmt::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Program { body })
    }
}

impl TryFrom<Pair<'_, Rule>> for Stmt {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        let rule = pair.as_rule();
        match rule {
            Rule::stmt => pair.into_inner().next().unwrap().try_into(),
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
            Rule::mod_def => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let body = inner.map(Stmt::try_from).collect::<Result<_, _>>()?;
                Ok(Stmt::ModuleDef { name, body })
            }
            Rule::expr => {
                let expr = pair.into_inner().next().unwrap().try_into()?;
                Ok(Stmt::Expr { expr })
            }
            _ => Err(anyhow!("expected statement")),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for Expr {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        let rule = pair.as_rule();
        match rule {
            Rule::expr => pair.into_inner().next().unwrap().try_into(),
            Rule::multiline_expr => {
                let body = pair
                    .into_inner()
                    .map(Stmt::try_from)
                    .collect::<Result<_, _>>()?;
                Ok(Expr::Multiline { body })
            }
            Rule::call => {
                let [path, args] = pair.into_inner().next_chunk().unwrap();
                let path = Path(
                    path.into_inner()
                        .map(|component| component.as_str().to_string())
                        .collect(),
                );
                let args = args
                    .into_inner()
                    .map(Expr::try_from)
                    .collect::<Result<_, _>>()?;
                Ok(Expr::Call { path, args })
            }
            Rule::literal => {
                let inner = pair.into_inner().next().unwrap();
                let value = match inner.as_rule() {
                    Rule::number => Literal::Number(inner.as_str().parse()?),
                    Rule::string => {
                        Literal::String(inner.into_inner().next().unwrap().as_str().to_string())
                    }
                    Rule::bool => Literal::Bool(inner.as_str() == "true"),
                    Rule::nil => Literal::Nil,
                    _ => unreachable!(),
                };
                Ok(Expr::Literal { value })
            }
            Rule::var_ref => {
                let name = pair.into_inner().next().unwrap().as_str().to_string();
                Ok(Expr::Variable { name })
            }
            _ => Err(anyhow!("expected expression")),
        }
    }
}
