use crate::ast::{AssignStmt, BinExpr, Expr, File, Ident, Literal, Op, UnaryExpr, VarDecl, IfStmt, Print};
use crate::opcode::{ByteCode, Opcode};
use crate::data::Data;
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

    fn if_stmt(&mut self, stmt: &Rc<IfStmt>) -> Result<(), CompileErr> {
        let if_stmt: &IfStmt = stmt.borrow();
        self.visit(&if_stmt.test)?;
        let then_jump = self.emit_jump(Opcode::Jeq);  
        for node in &if_stmt.body {
            self.visit(node)?;
        }
        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump)?;

        if let Some(Expr::ElseBlock(block)) = if_stmt.alternate.clone() {
            let nodes: &Vec<Expr> = block.borrow();
            for node in nodes {
                self.visit(&node.clone())?;
            }
        }
        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn print_expr(&mut self, expr: &Rc<Print>) -> Result<(), CompileErr> {
        let print_expr: &Print = expr.borrow();
        self.visit(&print_expr.expr)?;
        self.emit_opcode(Opcode::Print);

        Ok(())  
    }

    fn var_decl(&mut self, expr: &Rc<VarDecl>) -> Result<(), CompileErr> {
        let var_decl: &VarDecl = expr.borrow();

        self.visit(&var_decl.val)?;
        self.emit_opcode(Opcode::Save);
        self.code
            .constants
            .push(Data::String(var_decl.id.0.clone()));
        self.emit_byte(self.code.constants.len() as u8 - 1);

        Ok(())
    }

    fn assign_stmt(&mut self, expr: &Rc<AssignStmt>) -> Result<(), CompileErr> {
        let assign_stmt: &AssignStmt = expr.borrow();

        self.visit(&assign_stmt.val)?;
        self.emit_opcode(Opcode::Save);
        self.code
            .constants
            .push(Data::String(assign_stmt.id.0.clone()));
        self.emit_byte(self.code.constants.len() as u8 - 1);

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
                &Op::Modulo => self.emit_opcode(Opcode::Mod),
                &Op::Equals => self.emit_opcode(Opcode::Equal),
                &Op::NotEqual => self.emit_opcode(Opcode::NotEqual),
                &Op::Gte => self.emit_opcode(Opcode::Gte),
                &Op::Lte => self.emit_opcode(Opcode::Lte),
                &Op::Gt => self.emit_opcode(Opcode::Gt),
                &Op::Lt => self.emit_opcode(Opcode::Lt),
                _ => {}
            }
        }
        Ok(())
    }

    fn string(&mut self, val: &String) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::String(val.to_string()));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);
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

    fn ident(&mut self, id: &Ident) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::String((*id.0).to_string()));
        self.emit_opcode(Opcode::Load);
        self.emit_byte(idx);
        Ok(())
    }

    fn emit_jump(&mut self, opcode: Opcode) -> usize {
        self.emit_opcode(opcode);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        return self.code.opcodes.len() - 2;
    } 

    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileErr> {
        //#![allow(arithmetic_overflow)]
        let jump = (self.code.opcodes.len() - offset - 2) as u16;
        if jump > u16::MAX {
           return Err(CompileErr("Too much code to jump".to_string()))
        }
        self.code.opcodes[offset] = (jump as u16 >> 8) as u8 & 0xff;
        self.code.opcodes[offset + 1] = jump as u8 & 0xff;

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
            Expr::IfStmt(stmt) => self.if_stmt(stmt),
            Expr::Print(expr) => self.print_expr(expr),
            Expr::VarDecl(expr) => self.var_decl(expr),
            Expr::AssignStmt(expr) => self.assign_stmt(expr),
            Expr::BinExpr(expr) => self.binary(expr),
            Expr::UnaryExpr(expr) => self.unary(expr),
            Expr::Literal(Literal::Number(val)) => self.number(val),
            Expr::Literal(Literal::Boolean(val)) => self.boolean(val),
            Expr::Literal(Literal::String(val)) => self.string(val),
            Expr::Id(id) => self.ident(id),
            _ => Err(CompileErr("Compiler Error".to_string())),
        }
    }

    pub fn run(&mut self, file: &File) -> CompileRes {
        for node in &file.nodes {
            self.visit(node)?;
        }
        self.emit_opcode(Opcode::Halt);
        println!("{:?}", self.code.opcodes);
        return Ok(self.code.clone());
    }
}
