use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::Add;
use std::str::FromStr;

use anyhow::anyhow;
use pest::iterators::Pair;
use pest::{Parser, RuleType};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "scout.pest"]
struct ScoutParser;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Path(pub(crate) Vec<String>);

impl<'a, I> From<I> for Path
where
    I: IntoIterator<Item = &'a str>,
{
    fn from(value: I) -> Self {
        Self(value.into_iter().map(ToString::to_string).collect())
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone().join("::"))
    }
}

impl Add<String> for &Path {
    type Output = Path;

    fn add(self, rhs: String) -> Self::Output {
        let mut components = self.0.clone();
        components.push(rhs);
        Path(components)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Multiline {
        body: Vec<Stmt>,
    },
    Variable {
        name: String,
    },
    Literal {
        value: Literal,
    },
    Call {
        path: Path,
        args: Vec<Expr>,
    },
    If {
        condition: Box<Expr>,
        then_body: Vec<Stmt>,
        else_body: Option<Vec<Stmt>>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
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
    Break,
    Continue,
    Expr {
        expr: Expr,
    },
    ForLoop {
        iterator: String,
        target: Expr,
        body: Vec<Stmt>,
    },
    WhileLoop {
        condition: Expr,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[allow(dead_code)]
fn pretty_print_pair(pair: Pair<Rule>, indent_level: usize) {
    println!(
        "{}{:?} '{}'",
        "  ".repeat(indent_level),
        pair.as_rule(),
        pair.as_str().replace('\n', "\\n")
    );
    for child in pair.into_inner() {
        pretty_print_pair(child, indent_level + 1);
    }
}

struct ByRule<'a, R>(HashMap<R, Vec<Pair<'a, R>>>);

impl<'a, R: Eq + Hash> ByRule<'a, R> {
    pub fn pop(&mut self, rule: R) -> Option<Pair<'a, R>> {
        self.0.get_mut(&rule).and_then(Vec::pop)
    }
}

#[test]
fn test_programs() {
    let ast: Program = "".parse().unwrap();
    assert_eq!(ast, Program { body: Vec::new() });
    let ast: Program = "std::print(1)\n".parse().unwrap();
    assert_eq!(
        ast,
        Program {
            body: vec![Stmt::Expr {
                expr: Expr::Call {
                    path: Path::from(["std", "print"]),
                    args: vec![Expr::Literal {
                        value: Literal::Number(1.0),
                    }]
                }
            }]
        }
    );
}

trait PairExt<'a, R>
where
    R: RuleType,
{
    fn as_string(&self) -> String;
    fn inner_as_strings(self) -> Vec<String>;
    fn into_single_inner(self) -> Option<Pair<'a, R>>;
    fn into_by_rule(self) -> ByRule<'a, R>;
    fn try_map_inner<T, Col>(self) -> Result<Col, T::Error>
    where
        T: TryFrom<Pair<'a, R>>,
        Col: FromIterator<T>;
    fn map_inner<T, Col>(self) -> Col
    where
        T: From<Pair<'a, R>>,
        Col: FromIterator<T>;
    fn extract_rules<const N: usize>(self, rules: [R; N]) -> [Option<Pair<'a, R>>; N];
}

impl<'a, R> PairExt<'a, R> for Pair<'a, R>
where
    R: RuleType,
{
    fn as_string(&self) -> String {
        self.as_str().to_string()
    }

    fn inner_as_strings(self) -> Vec<String> {
        self.into_inner().map(|pair| pair.as_string()).collect()
    }

    fn into_single_inner(self) -> Option<Pair<'a, R>> {
        self.into_inner().next()
    }

    fn into_by_rule(self) -> ByRule<'a, R> {
        let mut map = HashMap::<_, Vec<_>>::new();
        for pair in self.into_inner() {
            map.entry(pair.as_rule()).or_default().push(pair);
        }
        ByRule(map)
    }

    fn try_map_inner<T, Col>(self) -> Result<Col, T::Error>
    where
        T: TryFrom<Pair<'a, R>>,
        Col: FromIterator<T>,
    {
        self.into_inner().map(TryFrom::try_from).collect()
    }

    fn map_inner<T, Col>(self) -> Col
    where
        T: From<Pair<'a, R>>,
        Col: FromIterator<T>,
    {
        self.into_inner().map(From::from).collect()
    }

    fn extract_rules<const N: usize>(self, rules: [R; N]) -> [Option<Pair<'a, R>>; N] {
        let mut by_rule = self.into_by_rule();
        rules.map(|rule| by_rule.pop(rule))
    }
}

impl FromStr for Program {
    type Err = anyhow::Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let pair = ScoutParser::parse(Rule::program, source)?.next().unwrap();
        // pretty_print_pair(pair.clone(), 0);
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
            .filter(|pair| pair.as_rule() != Rule::EOI && pair.as_str() != "\n")
            .map(Stmt::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Program { body })
    }
}

impl TryFrom<Pair<'_, Rule>> for Stmt {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::stmt_line | Rule::stmt | Rule::top_stmt | Rule::top_stmt_line => {
                pair.into_single_inner().unwrap().try_into()
            }
            Rule::func_def => {
                let [name, params, body] =
                    pair.extract_rules([Rule::ident, Rule::param_list, Rule::expr]);
                let name = name.unwrap().as_string();
                let params = params.unwrap().inner_as_strings();
                let body = body.unwrap().try_into()?;

                Ok(Stmt::FunctionDef { name, params, body })
            }
            Rule::for_loop => {
                let [iterator, target, body] =
                    pair.extract_rules([Rule::ident, Rule::expr, Rule::body]);
                let iterator = iterator.unwrap().as_string();
                let target = target.unwrap().try_into()?;
                let body = body.unwrap().try_map_inner()?;
                Ok(Stmt::ForLoop {
                    iterator,
                    target,
                    body,
                })
            }
            Rule::infinite_loop => {
                let body = pair.into_single_inner().unwrap().try_map_inner()?;
                Ok(Stmt::WhileLoop {
                    condition: Expr::Literal {
                        value: Literal::Bool(true),
                    },
                    body,
                })
            }
            Rule::return_stmt => {
                let retval = pair.into_single_inner().unwrap().try_into()?;
                Ok(Stmt::Return { retval })
            }
            Rule::break_stmt => Ok(Stmt::Break),
            Rule::continue_stmt => Ok(Stmt::Continue),
            Rule::var_def => {
                let [name, value] = pair.extract_rules([Rule::ident, Rule::expr]);
                let name = name.unwrap().as_string();
                let value = value.unwrap().try_into()?;
                Ok(Stmt::VariableDef { name, value })
            }
            Rule::mod_def => {
                let [name, body] = pair.extract_rules([Rule::ident, Rule::top_body]);
                let name = name.unwrap().as_string();
                let body = body.unwrap().try_map_inner()?;
                Ok(Stmt::ModuleDef { name, body })
            }
            Rule::expr => {
                let expr = pair.into_single_inner().unwrap().try_into()?;
                Ok(Stmt::Expr { expr })
            }
            Rule::while_loop => {
                let [condition, body] = pair.extract_rules([Rule::expr, Rule::body]);
                let condition = condition.unwrap().try_into()?;
                let body = body.unwrap().try_map_inner()?;
                Ok(Stmt::WhileLoop { condition, body })
            }
            rule => Err(anyhow!("expected statement: {rule:?}")),
        }
    }
}

#[test]
fn binary_ops() {
    let program: Program = "1 + 2\n".parse().unwrap();
    assert_eq!(
        program,
        Program {
            body: vec![Stmt::Expr {
                expr: Expr::Binary {
                    lhs: Box::new(Expr::Literal {
                        value: Literal::Number(1.0)
                    }),
                    op: BinaryOp::Add,
                    rhs: Box::new(Expr::Literal {
                        value: Literal::Number(2.0)
                    })
                }
            }]
        }
    )
}

impl TryFrom<Pair<'_, Rule>> for Expr {
    type Error = anyhow::Error;

    fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::expr | Rule::expr_atom | Rule::grouping => {
                pair.into_single_inner().unwrap().try_into()
            }
            Rule::multiline_expr => Ok(Expr::Multiline {
                body: pair.try_map_inner()?,
            }),
            Rule::expr_bin => {
                let [lhs, bin_op, rhs] =
                    pair.extract_rules([Rule::expr_atom, Rule::bin_op, Rule::expr_bin]);
                let lhs = lhs.unwrap().try_into()?;
                if let (Some(bin_op), Some(rhs)) = (bin_op, rhs) {
                    let op = match bin_op.into_single_inner().unwrap().as_rule() {
                        Rule::add => BinaryOp::Add,
                        Rule::subtract => BinaryOp::Subtract,
                        Rule::multiply => BinaryOp::Multiply,
                        Rule::divide => BinaryOp::Divide,
                        Rule::less => BinaryOp::Less,
                        Rule::greater => BinaryOp::Greater,
                        Rule::less_equal => BinaryOp::LessEqual,
                        Rule::greater_equal => BinaryOp::GreaterEqual,
                        Rule::equal => BinaryOp::Equal,
                        Rule::not_equal => BinaryOp::NotEqual,
                        rule => unreachable!("{rule:?}"),
                    };
                    let rhs = Box::new(rhs.try_into()?);
                    return Ok(Expr::Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs,
                    });
                }
                Ok(lhs)
            }
            Rule::call => {
                let [path, args] = pair.extract_rules([Rule::path, Rule::arg_list]);
                let path = Path(path.unwrap().inner_as_strings());
                let args = args.unwrap().into_single_inner().unwrap().try_map_inner()?;
                Ok(Expr::Call { path, args })
            }
            Rule::if_expr => {
                let [condition, then_body, else_body] =
                    pair.extract_rules([Rule::expr, Rule::then_body, Rule::else_body]);
                let condition = Box::new(condition.unwrap().try_into()?);
                let then_body = then_body.unwrap().try_map_inner()?;
                let else_body = else_body.map(PairExt::try_map_inner).transpose()?;

                Ok(Expr::If {
                    condition,
                    then_body,
                    else_body,
                })
            }
            Rule::literal => {
                let value = pair.into_single_inner().unwrap();
                let value = match value.as_rule() {
                    Rule::number => Literal::Number(value.as_str().parse()?),
                    Rule::string => Literal::String(value.into_single_inner().unwrap().as_string()),
                    Rule::bool => Literal::Bool(value.as_str() == "true"),
                    Rule::nil => Literal::Nil,
                    Rule::array => {
                        Literal::Array(value.into_single_inner().unwrap().try_map_inner()?)
                    }
                    _ => unreachable!(),
                };
                Ok(Expr::Literal { value })
            }
            Rule::var_ref => {
                let name = pair.into_single_inner().unwrap().as_string();
                Ok(Expr::Variable { name })
            }
            rule => Err(anyhow!("expected expression: {rule:?}")),
        }
    }
}
