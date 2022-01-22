use crate::common::{ByteCode, Opcode};
use crate::common::{Data, Function, NativeFun};
use crate::compiler::{ASTNode, BinExpr, Expr, Ident, Op, ScriptFun, Stmt, AST};
use crate::core::{ffi_core, FFI};

#[derive(Debug)]
pub struct CompileErr(pub String);

pub type CompileRes = Result<ByteCode, CompileErr>;

pub struct Compiler {
    locals: Locals,
    ffi: FFI,
    function: Function,
}

impl Compiler {
    pub fn build() -> Compiler {
        Compiler {
            locals: Locals::new(),
            ffi: ffi_core(),
            function: Function::empty(),
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
        self.locals.depth += 1;
    }

    fn exit_scope(&mut self) {
        self.locals.depth -= 1;

        while self.locals.locals_count > 0
            && self.locals.locals[self.locals.locals_count - 1].depth > self.locals.depth
        {
            self.emit_byte(Opcode::Del as u8);
            self.locals.locals_count -= 1;
            self.locals.locals.pop();
        }
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
        let loop_start = self.function.chunk.opcodes.len();

        self.visit(&ASTNode::from(*block))?;

        self.emit_loop(loop_start);

        Ok(())
    }

    fn while_stmt(&mut self, condition: Expr, block: Box<Stmt>) -> Result<(), CompileErr> {
        let loop_start = self.function.chunk.opcodes.len();
        self.visit(&ASTNode::from(condition))?;

        let jump = self.emit_jump(Opcode::Jeq);
        self.visit(&ASTNode::from(*block))?;
        self.emit_loop(loop_start);

        self.patch_jump(jump)?;

        Ok(())
    }

    fn fun_decl(&mut self, mut fun: Box<ScriptFun>) -> Result<(), CompileErr> {
        let mut compiler = Compiler::build();

        compiler.enter_scope();

        for param in fun.params.iter() {
            compiler.add_local(&param.name);
        }

        // This is kind of a hack
        if let Stmt::Block(mut stmts, span) = fun.body {
            stmts.push(Stmt::Return(Expr::Unit(span.clone()), span.clone()));
            fun.body = Stmt::Block(stmts, span);
        }

        let chunk = compiler
            .run(&AST::new(
                vec![ASTNode::from(fun.body.clone())],
                fun.body.span(),
            ))
            .unwrap();
        
        compiler.exit_scope();

        let fun = Function::new(fun.name.name, fun.params.len(), chunk.chunk);

        let name = fun.name.clone();

        let offset = self.emit_constant(Data::Function(fun));

        self.emit_opcode(Opcode::Const);
        self.emit_byte(offset as u8);

        if self.locals.depth > 0 {
            self.add_local(&name);
        } else {
            let index = self.emit_indent(name);
            self.declare_variable(index);
        }

        Ok(())
    }

    fn return_stmt(&mut self, expr: Expr) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr))?;
        self.emit_opcode(Opcode::Return);

        let locals = self
            .locals
            .locals
            .iter()
            .filter(|l| l.depth == self.locals.depth - 1)
            .collect::<Vec<&Local>>()
            .len();
        self.locals.locals_count -= locals;
        for _ in 0..locals {
            self.locals.locals.pop();
        }
        self.emit_byte(locals as u8);

        Ok(())
    }

    fn var_decl(&mut self, ident: Ident, expr: Expr) -> Result<(), CompileErr> {
        if self.locals.depth > 0 {
            self.visit(&ASTNode::from(expr))?;
            self.add_local(&ident.name);
        } else {
            let global = self.emit_indent(ident.name);

            self.visit(&ASTNode::from(expr))?;

            self.declare_variable(global);
        }

        Ok(())
    }

    fn declare_variable(&mut self, global: usize) {
        if self.locals.depth != 0 {
            return;
        }
        self.emit_opcode(Opcode::DefGlobal);
        self.emit_byte(global as u8);
    }

    fn assign_stmt(&mut self, ident: Ident, expr: Expr) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr))?;

        self.save_variable(&ident.name);

        Ok(())
    }

    fn expression(&mut self, expr: Expr) -> Result<(), CompileErr> {
        self.visit(&ASTNode::from(expr))?;
        self.emit_opcode(Opcode::Del);

        Ok(())
    }

    fn fun_call(&mut self, ident: Box<Expr>, args: Box<Vec<Expr>>) -> Result<(), CompileErr> {
        let ffi = &mut self.ffi.clone();
        if let Expr::Identifier(id) = *ident {
            match ffi.get(&id.name) {
                Some(fun) => {
                    for arg in args.iter().rev() {
                        self.visit(&ASTNode::from(arg))?;
                    }

                    let index = self.function.chunk.constants.len() as u8;
                    let fun_obj = Data::NativeFun(Box::new(NativeFun::new(
                        &id.name,
                        args.len(),
                        fun.clone(),
                    )));
                    self.emit_constant(fun_obj);
                    self.emit_opcode(Opcode::Const);
                    self.emit_byte(index);

                    self.emit_opcode(Opcode::Call);
                }
                None => {
                    for arg in args.iter() {
                        self.visit(&ASTNode::from(arg))?;
                    }

                    self.identifier(id)?;
                    self.emit_opcode(Opcode::Call);
                }
            }
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
        let idx = self.function.chunk.constants.len() as u8;
        self.emit_indent(val);
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);
        Ok(())
    }

    fn number(&mut self, val: f64) -> Result<(), CompileErr> {
        let idx = self.function.chunk.constants.len() as u8;
        self.emit_constant(Data::Number(val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn boolean(&mut self, val: bool) -> Result<(), CompileErr> {
        let idx = self.function.chunk.constants.len() as u8;
        self.emit_constant(Data::Boolean(val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn unit(&mut self) -> Result<(), CompileErr> {
        let idx = self.function.chunk.add_constant(Data::Unit);
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx as u8);

        Ok(())
    }

    fn _null(&mut self) -> Result<(), CompileErr> {
        let idx = self.function.chunk.constants.len() as u8;
        self.emit_constant(Data::Unit);
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn identifier(&mut self, id: Ident) -> Result<(), CompileErr> {
        if self.locals.depth == 0 {
            let index = self.emit_indent(id.name);
            self.emit_opcode(Opcode::GetGlobal);
            self.emit_byte(index as u8);

            return Ok(());
        }

        let _ = match self.resolve_local(&id.name) {
            Some(index) => {
                self.emit_opcode(Opcode::LoadLocal);
                self.emit_byte(index as u8);
            }
            None => {
                let index = self.emit_indent(id.name);
                self.emit_opcode(Opcode::GetGlobal);
                self.emit_byte(index as u8);
            }
        };

        Ok(())
    }

    fn save_variable(&mut self, name: &str) {
        let _ = match self.resolve_local(&name) {
            Some(index) => {
                self.emit_opcode(Opcode::SaveLocal);
                self.emit_byte(index as u8);
            }
            None => {
                let index = self.emit_indent(name.to_string());
                self.emit_opcode(Opcode::SetGlobal);
                self.emit_byte(index as u8);
            }
        };
    }

    fn add_local(&mut self, name: &str) {
        self.locals.add_local(name.to_string());
    }

    fn resolve_local(&mut self, id: &str) -> Option<usize> {
        let mut index = self.locals.locals_count;
        while index > 0 {
            let local = &self.locals.locals[index - 1];

            if local.name == id {
                return Some(index - 1);
            }

            index -= 1;
        }

        None
    }

    fn emit_indent(&mut self, value: String) -> usize {
        self.function.chunk.identifier(value)
    }

    fn emit_constant(&mut self, constant: Data) -> usize {
        return self.function.chunk.add_constant(constant);
    }

    fn emit_loop(&mut self, count: usize) {
        self.emit_opcode(Opcode::Loop);

        let offset = self.function.chunk.opcodes.len() - count + 2;

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    fn emit_jump(&mut self, opcode: Opcode) -> usize {
        self.emit_opcode(opcode);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        return self.function.chunk.opcodes.len() - 2;
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileErr> {
        let jump = (self.function.chunk.opcodes.len() - offset - 2) as u16;
        if jump > u16::MAX {
            return Err(CompileErr("Too much code to jump".to_string()));
        }
        self.function.chunk.opcodes[offset] = (jump as u16 >> 8) as u8 & 0xff;
        self.function.chunk.opcodes[offset + 1] = jump as u8 & 0xff;

        Ok(())
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        let byte = opcode as u8;
        self.emit_byte(byte);
    }

    fn emit_byte(&mut self, opcode: u8) {
        self.function.chunk.opcodes.push(opcode);
    }

    fn visit(&mut self, node: &ASTNode) -> Result<(), CompileErr> {
        match node.clone() {
            ASTNode::Stmt(stmt) => match stmt.clone() {
                Stmt::IfStatement(expr, block, _) => self.if_stmt(expr, block),
                Stmt::WhileStatement(expr, block, _) => self.while_stmt(expr, block),
                Stmt::LoopStatement(block, _) => self.loop_stmt(block),
                Stmt::Block(stmts, _) => self.block(&stmts),
                Stmt::VarDeclaration(ident, expr, _) => self.var_decl(ident, expr),
                Stmt::ConDeclaration(ident, expr, _) => self.var_decl(ident, expr),
                Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
                Stmt::ScriptFun(fun, _) => self.fun_decl(fun),
                Stmt::Return(expr, _) => self.return_stmt(expr),
                Stmt::Expr(expr) => self.expression(expr),
            },
            ASTNode::Expr(expr) => match expr.clone() {
                Expr::Number(val, _) => self.number(val),
                Expr::String(val, _) => self.string(val),
                Expr::Boolean(val, _) => self.boolean(val),
                Expr::Unit(_) => self.unit(),
                Expr::Identifier(ident) => self.identifier(ident),
                Expr::FunCall(ident, args, _) => self.fun_call(ident, args),
                Expr::BinExpr(expr, _) => self.binary(expr),
                Expr::UnaryExpr(op, expr, _) => self.unary(op, expr),
                Expr::List(list, _) => self.list(list),
                Expr::Or(lhs, rhs, _) => self.or(lhs, rhs),
                Expr::And(lhs, rhs, _) => self.and(lhs, rhs),
            },
        }
    }

    pub fn run(&mut self, ast: &AST) -> Result<Function, CompileErr> {
        for node in &ast.nodes {
            self.visit(node)?;
        }

        self.emit_opcode(Opcode::Halt);

        return Ok(self.function.clone());
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
    pub name: String,
    depth: usize,
}
