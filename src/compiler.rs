use crate::ast::{
    AssignStmt, BinExpr, File, FuncCall, Ident, IfStmt, Literal, Op, Print, UnaryExpr, VarDecl, AST,
};
use crate::data::Data;
use crate::opcode::{ByteCode, Opcode};

use std::borrow::Borrow;
use std::rc::Rc;

#[derive(Debug)]
pub struct CompileErr(pub String);

pub type CompileRes = Result<ByteCode, CompileErr>;

pub struct Compiler {
    code: ByteCode,
    locals: Vec<Locals>,
}

impl Compiler {
    pub fn build() -> Compiler {
        Compiler {
            code: ByteCode {
                opcodes: vec![],
                constants: vec![],
            },
            locals: vec![],
        }
    }

    fn block(&mut self, block: &Vec<AST>) -> Result<(), CompileErr> {
        self.enter_scope();

        for node in block {
            self.visit(&node)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn enter_scope(&mut self) {
        self.locals.push(Locals::new());
    }

    fn exit_scope(&mut self) {
        let top = self.locals.len() - 1;
        while *&self.locals[top].locals_count != 0 {
            self.locals[top].locals.pop();
            self.locals[top].locals_count -= 1;
            self.emit_opcode(Opcode::Del);
        }

        self.locals.pop();
    }

    fn if_stmt(&mut self, stmt: &Rc<IfStmt>) -> Result<(), CompileErr> {
        let if_stmt: &IfStmt = stmt.borrow();
        self.visit(&if_stmt.test)?;
        let then_jump = self.emit_jump(Opcode::Jeq);

        self.visit(&if_stmt.body)?;
        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump)?;

        if if_stmt.alternate.is_some() {
            self.visit(&if_stmt.alternate.clone().unwrap())?;
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

        self.declare_variable(expr.id.0.clone())?;

        Ok(())
    }
    fn declare_variable(&mut self, ident: String) -> Result<(), CompileErr> {
        /*if self.locals.len() == 0 {
            return Ok(());
        };*/

        let top = self.locals.len() - 1;

        self.locals[top].add_local(ident);

        Ok(())
    }

    fn assign_stmt(&mut self, expr: &Rc<AssignStmt>) -> Result<(), CompileErr> {
        let assign_stmt: &AssignStmt = expr.borrow();

        self.visit(&assign_stmt.val)?;

        let index = self.resolve_local(expr.id.0.clone(), self.locals.len() - 1);

        self.emit_opcode(Opcode::SaveLocal);
        self.emit_byte(index as u8);

        Ok(())
    }

    fn func_call(&mut self, expr: &FuncCall) -> Result<(), CompileErr> {
        for arg in &expr.args {
            self.visit(arg)?;
        }
        let offset = self.code.constants.len() as u8;
        self.emit_opcode(Opcode::FFICall);
        self.visit(&expr.callee);
        //self.code.constants.push(Data::String(expr.ident.0.clone()));
        self.emit_byte(offset);

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
                &Op::And => self.emit_opcode(Opcode::And),
                &Op::Or => self.emit_opcode(Opcode::Or),
                _ => {}
            }
        }
        Ok(())
    }

    fn list(&mut self, list: &Vec<AST>) -> Result<(), CompileErr> {
        for item in list.iter().rev() {
            self.visit(item)?;
        }
        self.emit_opcode(Opcode::List);
        self.emit_byte(list.len() as u8);
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
        let index = self.resolve_local(id.0.clone(), self.locals.len() - 1);
        self.emit_opcode(Opcode::LoadLocal);
        self.emit_byte(index as u8);

        Ok(())
    }

    fn resolve_local(&mut self, id: String, depth: usize) -> usize {
        return self
            .locals
            .last()
            .unwrap()
            .locals
            .iter()
            .position(|l| l.name == id)
            .unwrap();
        /*let index = self.locals[depth].locals.iter().position(|l| l.name == id);
        if index.is_none() {
            self.resolve_local(id, depth - 1)
        } else {
            index.unwrap()
        }*/
        /*let mut index = 0;
        for (_, scope) in self.locals.iter().rev().enumerate() {
            match scope.locals.iter().position(|l| l.name == id) {
                None => {
                    index += scope.locals_count;
                    continue;
                }
                Some(idx) => {
                    index += idx;
                    break;
                }
            }
        }*/

        /*let mut scopes = self.locals.clone();//.iter().rev();
        scopes.reverse();
        println!("{:?}", scopes);
        for scope in scopes {
            let pos = scope.locals.iter().position(|l| l.name == id);
            if pos.is_some() {
                index += pos.unwrap();
                break;
            } else {
                index += scope.locals_count;
            }
        }
        //while i

        println!("{}", index);
        /*println!("Looking for variable offset...");
        for locals in self.locals.iter().rev() {
            println!("{:?}", locals.locals_count);
        }*/
        */
        //return index;
    }

    fn emit_jump(&mut self, opcode: Opcode) -> usize {
        self.emit_opcode(opcode);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        return self.code.opcodes.len() - 2;
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileErr> {
        let jump = (self.code.opcodes.len() - offset - 2) as u16;
        if jump > u16::MAX {
            return Err(CompileErr("Too much code to jump".to_string()));
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

    fn visit(&mut self, node: &AST) -> Result<(), CompileErr> {
        match node {
            AST::IfStmt(stmt) => self.if_stmt(stmt),
            AST::Block(block) => self.block(block),
            AST::Print(expr) => self.print_expr(expr),
            AST::VarDecl(expr) => self.var_decl(expr),
            AST::AssignStmt(expr) => self.assign_stmt(expr),
            AST::FuncCall(expr) => self.func_call(expr),
            AST::BinExpr(expr) => self.binary(expr),
            AST::UnaryExpr(expr) => self.unary(expr),
            AST::List(list) => self.list(list),
            AST::Literal(Literal::Number(val)) => self.number(val),
            AST::Literal(Literal::Boolean(val)) => self.boolean(val),
            AST::Literal(Literal::String(val)) => self.string(val),
            AST::Id(id) => self.ident(id),
            _ => Err(CompileErr("Compiler Error".to_string())),
        }
    }

    pub fn run(&mut self, file: &File) -> CompileRes {
        self.enter_scope();

        for node in &file.nodes {
            self.visit(node)?;
        }
        self.emit_opcode(Opcode::Halt);

        self.exit_scope();

        return Ok(self.code.clone());
    }
}

#[derive(Debug, Clone)]
struct Locals {
    locals: Vec<Local>,
    depth: usize,
    locals_count: usize,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            locals: vec![],
            depth: 0,
            locals_count: 0,
        }
    }

    pub fn add_local(&mut self, name: String) {
        let local = Local {
            name,
            depth: self.depth,
        };

        self.locals_count += 1;
        self.locals.push(local);
    }
}

#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: usize,
}
