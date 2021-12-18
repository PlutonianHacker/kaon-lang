use crate::ast::{BinExpr, Expr, File, Literal, Op};
use crate::opcode::{ByteCode, Opcode};
use std::borrow::Borrow;
use std::rc::Rc;

pub struct CompileErr(pub String);

pub type CompileRes = Result<ByteCode, CompileErr>;

#[derive(Debug)]
pub struct Compiler {
    code: ByteCode,
}

impl Compiler {
    pub fn build() -> Compiler {
        Compiler {
            code: ByteCode {
                opcodes: vec![],
                constants: vec![],
            },
        }
    }

    fn binary(&mut self, expr: &Rc<BinExpr>) -> Result<(), CompileErr> {
        #![allow(irrefutable_let_patterns)]
        if let BinExpr { op, lhs, rhs } = expr.borrow() {
            let _lhs = self.visit(rhs)?;
            let _rhs = self.visit(lhs)?;

            match op {
                &Op::Add => self.emit_opcode(Opcode::Add),
                &Op::Sub => self.emit_opcode(Opcode::Sub),
                &Op::Mul => self.emit_opcode(Opcode::Mul),
                &Op::Div => self.emit_opcode(Opcode::Div),
            }
        }
        Ok(())
    }

    fn number(&mut self, literal: &Literal) -> Result<(), CompileErr> {
        match literal {
            Literal::Number(val) => {
                let idx = self.code.constants.len() as u8;
                self.code.constants.push(*val);
                self.emit_opcode(Opcode::Const);
                self.emit_byte(idx);
                //self.emit_bytes([0, 0]);
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        let byte = u8::from(opcode);
        self.emit_byte(byte);
    }

    fn emit_bytes(&mut self, bytes: [u8; 2]) {
        self.code.opcodes.append(&mut bytes.to_vec());
    }

    fn emit_byte(&mut self, byte: u8) {
        self.code.opcodes.push(byte);
    }

    fn visit(&mut self, node: &Expr) -> Result<(), CompileErr> {
        match node {
            Expr::BinExpr(expr) => self.binary(expr),
            Expr::Literal(val) => self.number(val),
            _ => Err(CompileErr("Compiler Error".to_string())),
        }
    }

    pub fn run(&mut self, file: &File) -> CompileRes {
        for node in &file.nodes {
            self.visit(node)?;
        }

        self.emit_opcode(Opcode::Halt);

        return Ok(self.code.clone());
    }
}
