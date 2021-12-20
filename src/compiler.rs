use crate::ast::{BinExpr, Expr, File, Ident, Literal, Op, UnaryExpr, VarDecl};
use crate::opcode::{ByteCode, Opcode};
use crate::stack::Data;
use std::borrow::Borrow;
use std::rc::Rc;

#[derive(Debug)]
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

    fn var_decl(&mut self, expr: &Rc<VarDecl>) -> Result<(), CompileErr> {
        let var_decl: &VarDecl = expr.borrow();
        self.visit(&var_decl.val)?;
        //self.emit_opcode(Opcode::SetGlobal, )
        //self.emit_opcode(Opcode::SetGlobal(expr.id.0.clone()));
        Ok(())
    }

    fn unary(&mut self, expr: &Rc<UnaryExpr>) -> Result<(), CompileErr> {
        let unary_expr: &UnaryExpr = expr.borrow();
        match unary_expr.op {
            Op::Add => self.visit(&unary_expr.rhs)?,
            Op::Sub => {
                self.visit(&unary_expr.rhs)?;
                self.emit_opcode(Opcode::Negate);
            }
            Op::Not => {
                self.visit(&unary_expr.rhs)?;
                self.emit_opcode(Opcode::Not);
            }
            _ => {}
        };
        Ok(())
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
                &Op::Equals => self.emit_opcode(Opcode::Equal),
                &Op::NotEqual => self.emit_opcode(Opcode::NotEqual),
                &Op::GToEq => self.emit_opcode(Opcode::GToEq),
                &Op::LToEq => self.emit_opcode(Opcode::LToEq),
                &Op::Gt => self.emit_opcode(Opcode::Gt),
                &Op::Lt => self.emit_opcode(Opcode::Lt),
                _ => {}
            }
        }
        Ok(())
    }

    fn number(&mut self, val: &f64) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::Number(*val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn boolean(&mut self, val: &bool) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::Boolean(*val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn ident(&mut self, _id: &Ident) -> Result<(), CompileErr> {
        Ok(())
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        let byte = u8::from(opcode);
        self.emit_byte(byte);
    }

    fn _emit_bytes(&mut self, opcodes: [u8; 2]) {
        self.code.opcodes.append(&mut opcodes.to_vec());
    }

    fn emit_byte(&mut self, opcode: u8) {
        self.code.opcodes.push(opcode);
    }

    fn visit(&mut self, node: &Expr) -> Result<(), CompileErr> {
        match node {
            Expr::VarDecl(expr) => self.var_decl(expr),
            Expr::BinExpr(expr) => self.binary(expr),
            Expr::UnaryExpr(expr) => self.unary(expr),
            Expr::Literal(Literal::Number(val)) => self.number(val),
            Expr::Literal(Literal::Boolean(val)) => self.boolean(val),
            Expr::Id(id) => self.ident(id),
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
