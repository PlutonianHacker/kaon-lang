use crate::ast::{BinExpr, Expr, File, Op};
use std::borrow::Borrow;
use std::rc::Rc;

pub enum Opcode {
    Load,
    Add,
    Sub,
    Mul,
    Div,
}

impl From<Opcode> for u8 {
    fn from(opcode: Opcode) -> u8 {
        match opcode {
            Opcode::Load => 0,
            Opcode::Add => 1,
            Opcode::Sub => 2,
            Opcode::Mul => 3,
            Opcode::Div => 4,
        }
    }
}

#[derive(Debug)]
pub struct CompileErr(String);

type CompileRes = Result<ByteCode, CompileErr>;

#[derive(Debug, Clone)]
pub struct ByteCode {
    opcodes: Vec<u8>,
}

#[derive(Debug)]
pub struct Compiler { 
    code: ByteCode,
}

impl Compiler {
    pub fn build() -> Compiler {
        Compiler {
            code: ByteCode { opcodes: vec![] },
        }
    }

    fn binary(&mut self, expr: &Rc<BinExpr>) -> Result<(), CompileErr> {
        #![allow(irrefutable_let_patterns)]
        if let BinExpr { op, lhs, rhs } = expr.borrow() {
            let _lhs = self.visit(lhs)?;
            let _rhs = self.visit(rhs)?;

            match op {
                &Op::Add => self.emit(Opcode::Add),
                &Op::Sub => self.emit(Opcode::Sub),
                &Op::Mul => self.emit(Opcode::Mul),
                &Op::Div => self.emit(Opcode::Div),
            }
        }
        Ok(())
    }

    fn emit(&mut self, opcode: Opcode) {
        let byte = u8::from(opcode);
        self.code.opcodes.push(byte);
    }

    fn visit(&mut self, node: &Expr) -> Result<(), CompileErr> {
        match node {
            Expr::BinExpr(expr) => {
                //println!("{:?}", expr);
                self.binary(expr)?;
                Ok(())
            }
            _ => Err(CompileErr("Compiler Error".to_string())),
        }
    }

    pub fn run(&mut self, file: &File) -> CompileRes {
        for node in &file.nodes {
            self.visit(node)?;
        }

        return Ok(self.code.clone());
    }
}
