use crate::ast::{Expr, Path, Program, Stmt};
use crate::interpreter::Value;
use anyhow::bail;

pub enum Instruction {
    LocalGet { name: String },
    LocalSet { name: String },
    Call { path: Path },
    Push { value: Value },
    DefineModule { name: String },
    EndModule { name: String },
    DefineFunction { name: String, params: Vec<String> },
    EndFunction { name: String },
    Return { name: String },

    End { id: usize },
    If { id: usize },
    Else { id: usize },
    ForIn { iterator: String, id: usize },
    While { id: usize },

    Continue { id: usize },
    Break { id: usize },
}

type InstructionVec = Vec<Instruction>;
type CompileResult = anyhow::Result<()>;

#[derive(Default)]
struct Compiler {
    insts: InstructionVec,
    current_function: Option<String>,
    current_loop: Option<usize>,
    current_id: usize,
}

impl Compiler {
    fn compile_program(&mut self, Program { body }: Program) -> CompileResult {
        self.compile_stmts(body)
    }

    fn compile_stmts(&mut self, stmts: Vec<Stmt>) -> CompileResult {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn new_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }

    fn compile_stmt(&mut self, stmt: Stmt) -> CompileResult {
        match stmt {
            Stmt::ModuleDef { name, body } => {
                self.add(Instruction::DefineModule { name: name.clone() });
                self.compile_stmts(body)?;
                self.add(Instruction::DefineModule { name });
            }
            Stmt::FunctionDef { name, body, params } => {
                self.add(Instruction::DefineFunction {
                    name: name.clone(),
                    params,
                });
                self.compile_expr(body)?;
                self.add(Instruction::EndFunction { name });
            }
            Stmt::VariableDef { name, value } => {
                self.compile_expr(value)?;
                self.add(Instruction::LocalSet { name });
            }
            Stmt::Return { retval } => {
                let Some(name) = self.current_function.take() else {
                    bail!("`return` outside of function");
                };
                self.compile_expr(retval)?;
                self.add(Instruction::Return { name });
            }
            Stmt::Break => {
                let Some(id) = self.current_loop.take() else {
                    bail!("`break` outside of loop");
                };
                self.add(Instruction::Break { id });
            }
            Stmt::Continue => {
                let Some(id) = self.current_loop else {
                    bail!("`continue` outside of loop");
                };
                self.add(Instruction::Continue { id });
            }
            Stmt::Expr { expr } => {
                self.compile_expr(expr)?;
            }
            Stmt::ForLoop {
                iterator,
                target,
                body,
            } => {
                let id = self.new_id();
                self.compile_expr(target)?;
                self.add(Instruction::ForIn { iterator, id });
                self.compile_stmts(body)?;
                self.add(Instruction::End { id });
            }
            Stmt::WhileLoop { condition, body } => {
                let id = self.new_id();
                self.add(Instruction::While { id });
                self.compile_expr(condition)?;
                self.add(Instruction::End { id });
            }
        }
        Ok(())
    }

    fn add(&mut self, inst: Instruction) {
        self.insts.push(inst);
    }

    fn compile_expr(&mut self, expr: Expr) -> CompileResult {
        match expr {
            Expr::Multiline { .. } => {}
            Expr::Variable { .. } => {}
            Expr::Literal { .. } => {}
            Expr::Call { .. } => {}
            Expr::If { .. } => {}
            Expr::Binary { .. } => {}
        }
        Ok(())
    }

    fn finish(self) -> anyhow::Result<InstructionVec> {
        Ok(self.insts)
    }
}

impl TryFrom<Program> for InstructionVec {
    type Error = anyhow::Error;

    fn try_from(Program { body }: Program) -> Result<Self, Self::Error> {
        compile_stmts(body)
    }
}

fn compile_stmts(stmts: impl IntoIterator<Item = Stmt>) -> Result<InstructionVec, anyhow::Error> {
    Ok(stmts
        .into_iter()
        .map(TryFrom::try_from)
        .collect::<Result<Vec<InstructionVec>, _>>()?
        .into_iter()
        .flatten()
        .collect())
}

impl TryFrom<Stmt> for InstructionVec {
    type Error = anyhow::Error;

    fn try_from(value: Stmt) -> Result<Self, Self::Error> {
        let mut insts = Vec::new();
        match value {
            Stmt::ModuleDef { name, body } => {
                insts.push(Instruction::DefineModule { name: name.clone() });
                insts.extend(compile_stmts(body)?);
                insts.push(Instruction::DefineModule { name });
            }
            Stmt::FunctionDef { name, body, params } => {
                insts.push(Instruction::DefineFunction {
                    name: name.clone(),
                    params,
                });
                insts.extend(InstructionVec::try_from(body)?);
                insts.push(Instruction::EndFunction { name });
            }
            Stmt::VariableDef { name, value } => {
                insts.extend(InstructionVec::try_from(value)?);
                insts.push(Instruction::LocalSet { name });
            }
            Stmt::Return { .. } => {}
            Stmt::Break => {}
            Stmt::Continue => {}
            Stmt::Expr { .. } => {}
            Stmt::ForLoop { .. } => {}
            Stmt::WhileLoop { .. } => {}
        }
        Ok(insts)
    }
}

impl TryFrom<Expr> for InstructionVec {
    type Error = anyhow::Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        todo!()
    }
}
