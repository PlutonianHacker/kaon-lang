use crate::ast::{ASTNode, BinExpr, Expr, Ident, Op, Stmt, AST};
use crate::data::{Data, NativeFun};
use crate::opcode::{ByteCode, Opcode};
use crate::core::{FFI, ffi_core};

#[derive(Debug)]
pub struct CompileErr(pub String);

pub type CompileRes = Result<ByteCode, CompileErr>;

pub struct Compiler {
    code: ByteCode,
    locals: Vec<Locals>,
    ffi: FFI,
}

impl Compiler {
    pub fn build() -> Compiler {
        Compiler {
            code: ByteCode::new(),
            locals: vec![],
            ffi: ffi_core(),
        }
    }

    fn block(&mut self, block: &Vec<Stmt>) -> Result<(), CompileErr> {
        self.enter_scope();

        for node in block {
            self.visit(&ASTNode::from(node.clone()))?;
        }

        self.exit_scope();

        Ok(())
    }

    fn enter_scope(&mut self) {
        self.locals.push(Locals::new(self.locals.len()));
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

    fn if_stmt(
        &mut self,
        condition: Expr,
        block: Box<(Stmt, Option<Stmt>)>,
    ) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(condition))?;
        let then_jump = self.emit_jump(Opcode::Jeq);

        self.visit(&ASTNode::from(block.0))?;
        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump)?;

        if block.1.is_some() {
            self.visit(&ASTNode::from(block.1.unwrap()))?;
        }

        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn loop_stmt(&mut self, block: Box<Stmt>) -> Result<(), CompileErr> {
        let loop_start = self.code.opcodes.len();

        self.visit(&ASTNode::from(*block))?;

        self.emit_loop(loop_start);

        Ok(())
    }

    fn while_stmt(&mut self, condition: Expr, block: Box<Stmt>) -> Result<(), CompileErr> {
        let loop_start = self.code.opcodes.len();
        self.visit(&ASTNode::from(condition))?;

        let jump = self.emit_jump(Opcode::Jeq);
        self.visit(&ASTNode::from(*block))?;
        self.emit_loop(loop_start);

        self.patch_jump(jump)?;

        Ok(())
    }

    fn var_decl(&mut self, ident: Ident, expr: Expr) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr))?;

        self.declare_variable(ident.name)?;

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

    fn assign_stmt(&mut self, ident: Ident, expr: Expr) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr))?;

        let index = self.resolve_local(ident.name, self.locals.len() - 1);

        self.emit_opcode(Opcode::SaveLocal);
        self.emit_byte(index as u8);

        Ok(())
    }

    fn expression(&mut self, expr: Expr) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr))?;
        self.emit_opcode(Opcode::Del);

        Ok(())
    }

    fn fun_call(&mut self, ident: Box<Expr>, args: Box<Vec<Expr>>) -> Result<(), CompileErr> {
        for arg in args.iter().rev() {
            self.visit(&ASTNode::from(arg))?;
        }

        if let Expr::Identifier(id) = *ident {
            let index = self.code.constants.len() as u8;

            let fun = self.ffi.get(&id.name).unwrap().clone();
            let fun_obj = Data::NativeFun(Box::new(NativeFun::new(&id.name, args.len(), fun)));

            self.code.constants.push(fun_obj);
            self.emit_opcode(Opcode::Call);
            self.emit_byte(index);
        }

        Ok(())
    }

    fn or(&mut self, lhs: Box<Expr>, rhs: Box<Expr>) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(*rhs))?;
        self.visit(&ASTNode::from(*lhs))?;
        self.emit_opcode(Opcode::Or);

        Ok(())
    }

    fn and(&mut self, lhs: Box<Expr>, rhs: Box<Expr>) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(*rhs))?;
        self.visit(&ASTNode::from(*lhs))?;
        self.emit_opcode(Opcode::And);

        Ok(())
    }

    fn unary(&mut self, op: Op, expr: Box<Expr>) -> Result<(), CompileErr> {
        match op {
            Op::Add => self.visit(&ASTNode::from(*expr))?,
            Op::Subtract => {
                self.visit(&ASTNode::from(*expr))?;
                self.emit_opcode(Opcode::Negate);
            }
            Op::Bang => {
                self.visit(&ASTNode::from(*expr))?;
                self.emit_opcode(Opcode::Not);
            }
            _ => {}
        };
        Ok(())
    }

    fn binary(&mut self, expr: Box<BinExpr>) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr.rhs))?;
        self.visit(&ASTNode::from(expr.lhs))?;

        match &expr.op {
            &Op::Add => self.emit_opcode(Opcode::Add),
            &Op::Subtract => self.emit_opcode(Opcode::Sub),
            &Op::Multiply => self.emit_opcode(Opcode::Mul),
            &Op::Divide => self.emit_opcode(Opcode::Div),
            &Op::Remainder => self.emit_opcode(Opcode::Mod),
            &Op::EqualTo => self.emit_opcode(Opcode::Equal),
            &Op::NotEqual => self.emit_opcode(Opcode::NotEqual),
            &Op::GreaterThanEquals => self.emit_opcode(Opcode::Gte),
            &Op::LessThanEquals => self.emit_opcode(Opcode::Lte),
            &Op::GreaterThan => self.emit_opcode(Opcode::Gt),
            &Op::LessThan => self.emit_opcode(Opcode::Lt),
            _ => {}
        }
        Ok(())
    }

    fn list(&mut self, list: Box<Vec<Expr>>) -> Result<(), CompileErr> {
        for item in list.iter().rev() {
            self.visit(&ASTNode::from(item.clone()))?;
        }
        self.emit_opcode(Opcode::List);
        self.emit_byte(list.len() as u8);
        Ok(())
    }

    fn string(&mut self, val: String) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::String(val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);
        Ok(())
    }

    fn number(&mut self, val: f64) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::Number(val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn boolean(&mut self, val: bool) -> Result<(), CompileErr> {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::Boolean(val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn identifier(&mut self, id: Ident) -> Result<(), CompileErr> {
        let index = self.resolve_local(id.name, self.locals.len() - 1);
        self.emit_opcode(Opcode::LoadLocal);
        self.emit_byte(index as u8);

        Ok(())
    }

    fn resolve_local(&mut self, id: String, _depth: usize) -> usize {
        // lookup local variable by id. Start in the innermost level
        let mut depth = 0;
        for locals in self.locals.iter().rev() {
            //println!("{:#?}", locals);
            match locals.locals.iter().position(|l| l.name == id) {
                None => {
                    depth += locals.locals_count;
                }
                Some(position) => {
                    depth += position;
                }
            }
        }
        //println!("{:?}", depth);
        return depth;
        /*return self
        .locals
        .last()
        .unwrap()
        .locals
        .iter()
        .position(|l| l.name == id)
        .unwrap();*/
        /*/*let index = self.locals[depth].locals.iter().position(|l| l.name == id);
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
        //return index;*/
    }

    fn emit_loop(&mut self, count: usize) {
        self.emit_opcode(Opcode::Loop);

        let offset = self.code.opcodes.len() - count + 2;

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
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

    fn visit(&mut self, node: &ASTNode) -> Result<(), CompileErr> {
        match node.clone() {
            ASTNode::Stmt(stmt) => match stmt.clone() {
                Stmt::IfStatement(expr, block, _) => self.if_stmt(expr, block),
                Stmt::WhileStatement(expr, block, _) => self.while_stmt(expr, block),
                Stmt::LoopStatement(block, _) => self.loop_stmt(block),
                Stmt::Block(stmts, _) => self.block(&stmts),
                Stmt::VarDeclaration(ident, expr, _) => self.var_decl(ident, expr),
                Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
                Stmt::Expr(expr) => self.expression(expr),
            },
            ASTNode::Expr(expr) => match expr.clone() {
                Expr::Number(val, _) => self.number(val),
                Expr::String(val, _) => self.string(val),
                Expr::Boolean(val, _) => self.boolean(val),
                Expr::Identifier(ident) => self.identifier(ident),
                Expr::FunCall(ident, args, _) => self.fun_call(ident, args),
                Expr::BinExpr(expr, _) => self.binary(expr),
                Expr::UnaryExpr(op, expr, _) => self.unary(op, expr),
                Expr::List(list, _) => self.list(list),
                Expr::Or(lhs, rhs) => self.or(lhs, rhs),
                Expr::And(lhs, rhs) => self.and(lhs, rhs),
            },
        }
    }

    pub fn run(&mut self, ast: &AST) -> CompileRes {
        self.enter_scope();

        for node in &ast.nodes {
            self.visit(node)?;
        }
        self.emit_opcode(Opcode::Halt);

        self.exit_scope();

        //println!("{:?}", self.code);

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
    pub fn new(depth: usize) -> Self {
        Locals {
            locals: vec![],
            depth,
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
