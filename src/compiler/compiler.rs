use crate::common::{self, Captured, Function, Opcode, Span, Value};
use crate::compiler::{
    ASTNode, BinExpr, Class, Constructor, Expr, Ident, Op, Pass, Scope, ScriptFun, Stmt, AST,
};

#[derive(Clone)]
pub struct Loop {
    start_ip: usize,
    jump_placeholders: Vec<usize>,
}

impl Loop {
    pub fn new(start_ip: usize) -> Self {
        Loop {
            start_ip,
            jump_placeholders: Vec::default(),
        }
    }
}

pub enum CompileTarget {
    Script,
    Function,
    Constructor,
}

#[derive(Debug)]
pub struct CompileErr(pub String);

/// Kaon bytecode compiler.
pub struct Compiler {
    enclosing: Option<Box<Compiler>>,
    locals: Locals,
    upvalues: Upvalues,
    globals: Scope,
    function: Function,
    loop_stack: Vec<Loop>,
    compile_target: CompileTarget,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            enclosing: None,
            locals: Locals::new(),
            upvalues: Upvalues::new(),
            globals: Scope::new(),
            function: Function::empty(),
            loop_stack: Vec::new(),
            compile_target: CompileTarget::Script,
        }
    }

    pub fn with_typ(compile_target: CompileTarget) -> Self {
        Compiler {
            enclosing: None,
            locals: Locals::new(),
            upvalues: Upvalues::new(),
            globals: Scope::new(),
            function: Function::empty(),
            loop_stack: Vec::new(),
            compile_target,
        }
    }

    fn enter_scope(&mut self) {
        self.locals.depth += 1;
    }

    fn exit_scope(&mut self) {
        self.locals.depth -= 1;

        while self.locals.locals_count > 0
            && self.locals.locals[self.locals.locals_count - 1].depth > self.locals.depth
        {
            let local = self.locals.locals.pop();
            if local.is_some() && local.unwrap().is_captured {
                self.emit_opcode(Opcode::CloseUpValue);
            } else {
                self.emit_byte(Opcode::Del as u8);
            }
            self.locals.locals_count -= 1;
        }
    }

    fn declare_variable(&mut self, name: &str) {
        if self.locals.depth > 0 {
            self.add_local(name);
        } else {
            let index = self.emit_indent(name.to_string());
            self.declare_global(index);
        }
    }

    fn declare_global(&mut self, global: usize) {
        if self.locals.depth != 0 {
            return;
        }
        self.emit_opcode(Opcode::DefGlobal);
        self.emit_byte(global as u8);
    }

    fn save_variable(&mut self, name: &str) {
        let _ = match self.resolve_local(name) {
            Some(index) => {
                self.emit_opcode(Opcode::SaveLocal);
                self.emit_byte(index as u8);
            }
            None => match self.resolve_upvalue(name) {
                Some(index) => {
                    self.emit_opcode(Opcode::SaveUpValue);
                    self.emit_byte(index as u8);
                }
                None => {
                    let index = self.emit_indent(name.to_string());
                    self.emit_opcode(Opcode::SetGlobal);
                    self.emit_byte(index as u8);
                }
            },
        };
    }

    fn add_local(&mut self, name: &str) {
        self.locals.add_local(name.to_string());
    }

    fn resolve_local(&self, id: &str) -> Option<usize> {
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

    fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        self.enclosing.as_ref()?;

        let enclosing = &mut self.enclosing.as_deref_mut()?;

        let local = enclosing.resolve_local(name);
        if let Some(local) = local {
            self.enclosing.as_deref_mut()?.locals.locals[local].is_captured = true;
            return self.add_upvalue(local, true);
        }

        let upvalue = self.enclosing.as_deref_mut()?.resolve_upvalue(name);
        if let Some(upvalue) = upvalue {
            return self.add_upvalue(upvalue, false);
        }

        None
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> Option<usize> {
        Some(self.upvalues.add_upvalue(index, is_local))
    }

    fn leave_loop(&mut self) -> Result<(), CompileErr> {
        for offset in &self.current_loop()?.jump_placeholders.clone() {
            self.patch_jump(*offset)?;
        }

        self.loop_stack.pop();
        Ok(())
    }

    fn current_loop(&mut self) -> Result<&Loop, CompileErr> {
        self.loop_stack
            .last()
            .ok_or_else(|| CompileErr("missing loop information".to_string()))
    }

    fn emit_expression(&mut self, expr: &Expr) -> Result<(), CompileErr> {
        self.expression(&expr)?;
        self.emit_opcode(Opcode::Del);

        Ok(())
    }

    fn emit_closure(
        &mut self,
        name: &Ident,
        params: &Vec<Ident>,
        body: &Stmt,
        typ: CompileTarget,
    ) -> Result<(), CompileErr> {
        let compiler = Compiler::with_typ(typ);
        let enclosing = std::mem::replace(self, compiler);

        self.enclosing = Some(Box::new(enclosing));
        self.function.name = name.name.to_string();
        self.globals = self.enclosing.as_deref_mut().unwrap().globals.clone();

        self.enter_scope();

        for param in params.iter().rev() {
            self.add_local(&param.name);
        }

        let chunk = self
            .run(
                &AST::new(vec![ASTNode::from(body.clone())], body.span()),
                self.globals.clone(),
            )
            .unwrap();
        self.exit_scope();

        let enclosing = std::mem::replace(&mut self.enclosing, None);
        let nested = std::mem::replace(self, *enclosing.unwrap());

        let fun = Function::new(
            name.name.to_owned(),
            params.len(),
            chunk.chunk,
            nested.upvalues.upvalues,
        );

        let name = fun.name.clone();
        let offset = self.emit_constant(Value::Function(fun));

        self.emit_opcode(Opcode::Closure);
        self.emit_byte(offset as u8);

        self.declare_variable(&name);

        Ok(())
    }

    fn emit_indent(&mut self, value: String) -> usize {
        self.function.chunk.identifier(value)
    }

    fn emit_constant(&mut self, constant: Value) -> usize {
        self.function.chunk.add_constant(constant)
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
        self.function.chunk.opcodes.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileErr> {
        let jump = self.function.chunk.opcodes.len() - offset - 2;
        if jump > u16::MAX.into() {
            return Err(CompileErr("Too much code to jump".to_string()));
        }
        self.function.chunk.opcodes[offset] = (jump as u16 >> 8) as u8;
        self.function.chunk.opcodes[offset + 1] = jump as u8;

        Ok(())
    }

    fn emit_return(&mut self) {
        match self.compile_target {
            CompileTarget::Script => self.emit_opcode(Opcode::Halt),
            CompileTarget::Function => {
                self.emit_opcode(Opcode::Nil);
                self.emit_opcode(Opcode::Return);
            }
            CompileTarget::Constructor => {
                self.emit_opcode(Opcode::Return);
                self.emit_opcode(Opcode::Del);
            }
        }
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        let byte = opcode as u8;
        self.emit_byte(byte);
    }

    fn emit_byte(&mut self, opcode: u8) {
        self.function.chunk.opcodes.push(opcode);
    }

    pub fn run(&mut self, ast: &AST, globals: Scope) -> Result<Function, CompileErr> {
        self.globals = globals;

        for node in &ast.nodes {
            match node {
                ASTNode::Stmt(stmt) => self.statment(stmt)?,
                ASTNode::Expr(expr) => self.expression(expr)?,
            };
        }

        self.emit_return();

        Ok(self.function.clone())
    }
}

impl Pass<(), CompileErr> for Compiler {
    fn statment(&mut self, stmt: &Stmt) -> Result<(), CompileErr> {
        match stmt {
            Stmt::Block(stmts, _) => self.block(stmts),
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::VarDeclaration(ident, expr, _, _) => self.var_decl(ident, expr),
            Stmt::ConDeclaration(ident, expr, _, _) => self.con_decl(ident, expr),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
            Stmt::ScriptFun(fun, _) => self.fun(fun),
            Stmt::Class(class, _) => self.class(class),
            Stmt::Constructor(constructor, _) => self.constructor(constructor),
            Stmt::Return(expr, _) => self.return_stmt(expr),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.emit_expression(expr),
        }
    }

    fn block(&mut self, block: &[Stmt]) -> Result<(), CompileErr> {
        self.enter_scope();

        for node in block {
            self.statment(&node)?;
        }

        self.exit_scope();

        Ok(())
    }

    fn if_statement(
        &mut self,
        condition: &Expr,
        block: &(Stmt, Option<Stmt>),
    ) -> Result<(), CompileErr> {
        self.expression(condition)?;
        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_opcode(Opcode::Del);

        self.statment(&block.0)?;
        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump)?;
        self.emit_opcode(Opcode::Del);

        if let Some(block) = &block.1 {
            self.statment(block)?;
        }

        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn loop_statement(&mut self, block: &Stmt) -> Result<(), CompileErr> {
        let start_ip = self.function.chunk.opcodes.len();

        self.loop_stack.push(Loop::new(start_ip));

        self.statment(block)?;

        self.emit_loop(start_ip);
        self.leave_loop()?;

        Ok(())
    }

    fn while_statement(&mut self, condition: &Expr, block: &Stmt) -> Result<(), CompileErr> {
        let loop_start = self.function.chunk.opcodes.len();

        self.loop_stack.push(Loop::new(loop_start));

        self.expression(condition)?;
        let jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_opcode(Opcode::Del);

        self.statment(block)?;
        self.emit_loop(loop_start);

        self.leave_loop()?;

        self.patch_jump(jump)?;
        self.emit_opcode(Opcode::Del);

        Ok(())
    }

    fn break_stmt(&mut self) -> Result<(), CompileErr> {
        let exit_jump = self.emit_jump(Opcode::Jump);
        match self.loop_stack.last_mut() {
            Some(loop_) => loop_.jump_placeholders.push(exit_jump),
            None => {
                return Err(CompileErr(
                    "cannot use break statement outside of loop".to_string(),
                ))
            }
        };

        Ok(())
    }

    fn continue_stmt(&mut self) -> Result<(), CompileErr> {
        let loop_start = self.current_loop()?.start_ip;

        self.emit_loop(loop_start);

        Ok(())
    }

    fn fun(&mut self, fun: &ScriptFun) -> Result<(), CompileErr> {
        self.emit_closure(&fun.name, &fun.params, &fun.body, CompileTarget::Function)?;

        Ok(())
    }

    fn return_stmt(&mut self, expr: &Expr) -> Result<(), CompileErr> {
        self.expression(expr)?;
        self.emit_opcode(Opcode::Return);

        Ok(())
    }

    fn class(&mut self, class: &Class) -> Result<(), CompileErr> {
        for constructor in &class.constructors {
            self.statment(constructor)?;
        }

        for field in &class.fields {
            if let Stmt::VarDeclaration(name, init, _, _) = field {
                if let Some(init) = init {
                    self.expression(init)?;
                } else {
                    self.nil()?;
                }

                let index = self.emit_indent(name.name.to_owned());

                self.emit_opcode(Opcode::Const);
                self.emit_byte(index as u8);
            }
        }

        let offset = self.emit_constant(Value::Class(common::Class::new(class.name())));

        self.emit_opcode(Opcode::Class);
        self.emit_byte(offset as u8);

        self.emit_byte(class.constructors.len() as u8);
        self.emit_byte(class.fields.len() as u8);

        self.declare_variable(&class.name());

        Ok(())
    }

    fn constructor(&mut self, constructor: &Constructor) -> Result<(), CompileErr> {
        self.emit_closure(
            &constructor.name,
            &constructor.params,
            &constructor.body,
            CompileTarget::Constructor,
        )?;

        self.identifier(&constructor.name)?;
        self.emit_opcode(Opcode::Constructor);

        Ok(())
    }

    fn var_decl(&mut self, ident: &Ident, expr: &Option<Expr>) -> Result<(), CompileErr> {
        let expr = if let Some(expr) = expr {
            expr.clone()
        } else {
            Expr::Nil(Span::empty())
        };

        if self.locals.depth > 0 {
            self.expression(&expr)?;
            self.add_local(&ident.name);
        } else {
            let global = self.emit_indent(ident.name.to_owned());

            self.expression(&expr)?;

            self.declare_global(global);
        }

        Ok(())
    }

    fn con_decl(&mut self, ident: &Ident, expr: &Expr) -> Result<(), CompileErr> {
        if self.locals.depth > 0 {
            self.expression(expr)?;
            self.add_local(&ident.name);
        } else {
            let global = self.emit_indent(ident.name.to_owned());

            self.expression(expr)?;

            self.declare_global(global);
        }

        Ok(())
    }

    fn assign_stmt(&mut self, ident: &Expr, expr: &Expr) -> Result<(), CompileErr> {
        self.expression(expr)?;

        if let Expr::Identifier(name) = ident {
            self.save_variable(&name.name);
        }

        Ok(())
    }

    fn fun_call(&mut self, ident: &Expr, args: &[Expr]) -> Result<(), CompileErr> {
        for arg in args.iter().rev() {
            self.expression(arg)?;
        }

        self.expression(ident)?;

        self.emit_opcode(Opcode::Call);
        self.emit_byte(args.len() as u8);

        Ok(())
    }

    fn member_expr(&mut self, object: &Expr, property: &Expr) -> Result<(), CompileErr> {
        self.expression(object)?;

        if let Expr::Identifier(id) = property {
            let index = self.emit_constant(Value::String(id.name.to_owned()));
            self.emit_opcode(Opcode::Get);
            self.emit_byte(index as u8);
        }

        Ok(())
    }

    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), CompileErr> {
        self.expression(lhs)?;

        let jump_offset = self.emit_jump(Opcode::JumpIfTrue);
        self.emit_opcode(Opcode::Del);
        self.expression(rhs)?;

        self.patch_jump(jump_offset)?;

        Ok(())
    }

    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), CompileErr> {
        self.expression(lhs)?;

        let jump_offset = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_opcode(Opcode::Del);
        self.expression(rhs)?;

        self.patch_jump(jump_offset)?;

        Ok(())
    }

    fn binary_expr(&mut self, expr: &BinExpr) -> Result<(), CompileErr> {
        self.expression(&expr.rhs)?;
        self.expression(&expr.lhs)?;

        match expr.op {
            Op::Add => self.emit_opcode(Opcode::Add),
            Op::Subtract => self.emit_opcode(Opcode::Sub),
            Op::Multiply => self.emit_opcode(Opcode::Mul),
            Op::Divide => self.emit_opcode(Opcode::Div),
            Op::Remainder => self.emit_opcode(Opcode::Mod),
            Op::EqualTo => self.emit_opcode(Opcode::Equal),
            Op::NotEqual => self.emit_opcode(Opcode::NotEqual),
            Op::GreaterThanEquals => self.emit_opcode(Opcode::Gte),
            Op::LessThanEquals => self.emit_opcode(Opcode::Lte),
            Op::GreaterThan => self.emit_opcode(Opcode::Gt),
            Op::LessThan => self.emit_opcode(Opcode::Lt),
            _ => {}
        }

        Ok(())
    }

    fn unary_expr(&mut self, op: &Op, expr: &Expr) -> Result<(), CompileErr> {
        match op {
            Op::Add => self.expression(expr)?,
            Op::Subtract => {
                self.expression(expr)?;
                self.emit_opcode(Opcode::Negate);
            }
            Op::Bang => {
                self.expression(expr)?;
                self.emit_opcode(Opcode::Not);
            }
            _ => {}
        };

        Ok(())
    }

    fn list(&mut self, list: Vec<Expr>) -> Result<(), CompileErr> {
        for item in list.iter().rev() {
            self.expression(item)?;
        }
        self.emit_opcode(Opcode::List);
        self.emit_byte(list.len() as u8);

        Ok(())
    }

    fn tuple(&mut self, tuple: &[Expr]) -> Result<(), CompileErr> {
        for item in tuple.iter().rev() {
            self.expression(item)?;
        }

        self.emit_opcode(Opcode::BuildTuple);
        self.emit_byte(tuple.len() as u8);

        Ok(())
    }

    fn map(&mut self, map: &[(Expr, Expr)]) -> Result<(), CompileErr> {
        for (key, value) in map.iter().rev() {
            self.expression(value)?;
            if let Expr::Identifier(ident) = key {
                let index = self.emit_constant(Value::String(ident.name.clone()));
                self.emit_opcode(Opcode::Const);
                self.emit_byte(index as u8);
            }
        }

        self.emit_opcode(Opcode::BuildMap);
        self.emit_byte(map.len() as u8);

        Ok(())
    }

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<(), CompileErr> {
        self.expression(expr)?;
        self.expression(index)?;

        self.emit_opcode(Opcode::Index);

        Ok(())
    }

    fn self_expr(&mut self) -> Result<(), CompileErr> {
        Ok(())
    }

    fn string(&mut self, val: &str) -> Result<(), CompileErr> {
        let idx = self.function.chunk.constants.len() as u8;
        self.emit_indent(val.to_string());
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);
        Ok(())
    }

    fn number(&mut self, val: &f64) -> Result<(), CompileErr> {
        let idx = self.function.chunk.constants.len() as u8;
        self.emit_constant(Value::Number(*val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);

        Ok(())
    }

    fn boolean(&mut self, val: &bool) -> Result<(), CompileErr> {
        if *val {
            self.emit_opcode(Opcode::True);
        } else {
            self.emit_opcode(Opcode::False);
        }

        Ok(())
    }

    fn nil(&mut self) -> Result<(), CompileErr> {
        self.emit_opcode(Opcode::Nil);
        Ok(())
    }

    fn identifier(&mut self, id: &Ident) -> Result<(), CompileErr> {
        if self.locals.depth == 0 {
            let index = self.emit_indent(id.name.to_owned());
            self.emit_opcode(Opcode::GetGlobal);
            self.emit_byte(index as u8);

            return Ok(());
        }

        let _ = match self.resolve_local(&id.name) {
            Some(index) => {
                self.emit_opcode(Opcode::LoadLocal);
                self.emit_byte(index as u8);
            }
            None => match self.resolve_upvalue(&id.name) {
                Some(index) => {
                    self.emit_opcode(Opcode::LoadUpValue);
                    self.emit_byte(index as u8);
                }
                None => {
                    let index = self.emit_indent(id.name.to_owned());
                    self.emit_opcode(Opcode::GetGlobal);
                    self.emit_byte(index as u8);
                }
            },
        };

        Ok(())
    }

    fn type_spec(&mut self, _typ: &Ident) -> Result<(), CompileErr> {
        Ok(())
    }
}

#[derive(Debug)]
struct Upvalues {
    upvalues: Vec<Captured>,
    upvalues_count: usize,
}

impl Upvalues {
    pub fn new() -> Self {
        Upvalues {
            upvalues: Vec::new(),
            upvalues_count: 0,
        }
    }

    pub fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        for (pos, upvalue) in self.upvalues.iter().enumerate() {
            if let Captured::Local(local_idx) = upvalue {
                if is_local && index == *local_idx {
                    return pos;
                }
            }
            if let Captured::NonLocal(nonlocal_idx) = upvalue {
                if !is_local && index == *nonlocal_idx {
                    return pos;
                }
            }
        }

        let upvalue = match is_local {
            true => Captured::Local(index),
            false => Captured::NonLocal(index),
        };

        self.upvalues_count += 1;
        self.upvalues.push(upvalue);

        index
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
            is_captured: false,
        };

        self.locals_count += 1;
        self.locals.push(local);
    }
}

#[derive(Debug, Clone)]
struct Local {
    pub name: String,
    pub depth: usize,
    pub is_captured: bool,
}
