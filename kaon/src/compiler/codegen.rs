use crate::common::{Captured, Function, Opcode, Span, Value};
use crate::compiler::{
    ASTNode, BinExpr, Class, Constructor, Expr, Ident, Op, Pass, Scope, ScriptFun, Stmt, TypePath,
    AST,
};

use std::rc::Rc;

/// Track the state of a loop.
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

/// Track a function's upvalues.
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

/// A struct for tracking local variables in a function's scope.
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

/// A local variable.
#[derive(Debug, Clone)]
struct Local {
    pub name: String,
    pub depth: usize,
    pub is_captured: bool,
}

/// Track the current [`Function`] being compiled.
pub struct Frame {
    function: Function,
    function_typ: CompileTarget,
    upvalues: Upvalues,
    locals: Locals,
}

impl Frame {
    pub fn new(target: CompileTarget, name: String, arity: usize) -> Self {
        Self {
            function: Function {
                name,
                arity,
                ..Function::default()
            },
            function_typ: target,
            upvalues: Upvalues::new(),
            locals: Locals::new(),
        }
    }

    /// A helper method for creating a script [`Function`].
    pub fn script() -> Self {
        Self {
            function: Function::script(),
            function_typ: CompileTarget::Script,
            upvalues: Upvalues::new(),
            locals: Locals::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CompileTarget {
    Script,
    Function,
    Constructor,
    Method,
    Module,
}

#[derive(Debug)]
pub struct CompileErr(pub String);

/// Kaon bytecode compiler.
pub struct Compiler {
    /// The global scope.
    globals: Scope,
    /// The compiler's function stack.
    frames: Vec<Frame>,
    /// A stack for tracking loop information.
    loop_stack: Vec<Loop>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            globals: Scope::new(),
            frames: Vec::new(),
            loop_stack: Vec::new(),
        }
    }

    /// Enter a new scope.
    fn enter_scope(&mut self) {
        self.current_mut_frame().locals.depth += 1;
    }

    /// Leave the scope, and clean up the mess.
    fn exit_scope(&mut self) {
        self.current_mut_frame().locals.depth -= 1;

        while self.current_frame().locals.locals_count > 0
            && self.current_frame().locals.locals[self.current_frame().locals.locals_count - 1]
                .depth
                > self.current_frame().locals.depth
        {
            let local = self.current_mut_frame().locals.locals.pop();
            if local.is_some() && local.unwrap().is_captured {
                self.emit_opcode(Opcode::CloseUpValue);
            } else {
                self.emit_byte(Opcode::Del as u8);
            }

            self.current_mut_frame().locals.locals_count -= 1;
        }
    }

    /// Declare a new variable.
    fn declare_variable(&mut self, name: &str) {
        if self.current_frame().locals.depth > 0 {
            self.add_local(name);
        } else {
            let index = self.emit_indent(name.to_string());
            self.declare_global(index);
        }
    }

    /// Declare a new global variable.
    fn declare_global(&mut self, global: usize) {
        if self.current_frame().locals.depth != 0 {
            return;
        }
        self.emit_opcode(Opcode::DefGlobal);
        self.emit_byte(global as u8);
    }

    /// Update a variable's value.
    fn save_variable(&mut self, name: &str) {
        let frame = self.current_frame();
        let _ = match self.resolve_local(name, frame) {
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

    /// Push a new local variable onto the stack.
    fn add_local(&mut self, name: &str) {
        self.current_mut_frame().locals.add_local(name.to_string());
    }

    /// Lookup a local in the current scope.
    fn resolve_local(&self, id: &str, frame: &Frame) -> Option<usize> {
        let mut index = frame.locals.locals_count;
        while index > 0 {
            let local = &frame.locals.locals[index - 1];

            if local.name == id {
                return Some(index - 1);
            }

            index -= 1;
        }

        None
    }

    /// Find an upvalue, itererating backwards through each frame.
    fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        let mut frame_count = self.frames.len() - 1;

        while frame_count != 0 {
            let frame = self.frames.get(frame_count)?;
            let local = self.resolve_local(name, frame);

            let frame = &mut self.frames[frame_count];

            if let Some(local) = local {
                frame.locals.locals[local].is_captured = true;
                return self.add_upvalue(local, true);
            }

            frame_count -= 1;
        }

        None
    }

    /// Add an upvalue to the current frame.
    fn add_upvalue(&mut self, index: usize, is_local: bool) -> Option<usize> {
        Some(
            self.current_mut_frame()
                .upvalues
                .add_upvalue(index, is_local),
        )
    }

    /// Return a reference to the current frame.
    fn current_frame(&self) -> &Frame {
        let len = self.frames.len();
        &self.frames[len - 1]
    }

    /// Return a mutable reference to the current frame.
    fn current_mut_frame(&mut self) -> &mut Frame {
        let len = self.frame_count();
        &mut self.frames[len - 1]
    }

    /// Get the frame count.
    fn frame_count(&self) -> usize {
        self.frames.len()
    }

    /// Leave the current loop.
    fn leave_loop(&mut self) -> Result<(), CompileErr> {
        for offset in &self.current_loop()?.jump_placeholders.clone() {
            self.patch_jump(*offset)?;
        }

        self.loop_stack.pop();
        Ok(())
    }

    /// Get the current loop.
    fn current_loop(&mut self) -> Result<&Loop, CompileErr> {
        self.loop_stack
            .last()
            .ok_or_else(|| CompileErr("missing loop information".to_string()))
    }

    /// Push a new functon onto the stack.
    fn enter_function(&mut self, frame: Frame) {
        self.frames.push(frame);

        self.enter_scope();
    }

    /// Pop the frame and return its function.
    fn exit_function(&mut self) -> Function {
        self.exit_scope();
        self.emit_return();

        let mut frame = self.frames.pop().unwrap();
        frame.function.captures = frame.upvalues.upvalues;

        frame.function
    }

    /// Emit a closure.
    fn compile_function(
        &mut self,
        name: &Ident,
        params: &[Ident],
        body: &Stmt,
        typ: CompileTarget,
    ) -> Result<(), CompileErr> {
        self.enter_function(Frame::new(typ, name.name.to_string(), params.len()));

        {
            for param in params.iter().rev() {
                self.add_local(&param.name);
            }

            if let Stmt::Block(stmt, _) = body {
                for stmt in &**stmt {
                    self.statment(stmt)?;
                }
            }
        }

        let fun = self.exit_function();

        let offset = self.emit_constant(Value::Function(Rc::new(fun)));

        self.emit_opcode(Opcode::Closure);
        self.emit_byte(offset as u8);

        self.declare_variable(&name.name);

        Ok(())
    }

    /// Compile an expression, and immediately pop it off the stack.
    fn emit_expression(&mut self, expr: &Expr) -> Result<(), CompileErr> {
        self.expression(expr)?;
        self.emit_opcode(Opcode::Del);

        Ok(())
    }

    /// Emit an identifier.
    fn emit_indent(&mut self, value: String) -> usize {
        self.current_mut_frame().function.chunk.identifier(value)
    }

    /// Emit a value.
    fn emit_constant(&mut self, constant: Value) -> usize {
        self.current_mut_frame()
            .function
            .chunk
            .add_constant(constant)
    }

    /// Emit a loop.
    fn emit_loop(&mut self, count: usize) {
        self.emit_opcode(Opcode::Loop);

        let offset = self.current_frame().function.chunk.opcodes.len() - count + 2;

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    /// Emit a jump opcode.
    fn emit_jump(&mut self, opcode: Opcode) -> usize {
        self.emit_opcode(opcode);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_frame().function.chunk.opcodes.len() - 2
    }

    /// Patch a jump.
    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileErr> {
        let jump = self.current_frame().function.chunk.opcodes.len() - offset - 2;
        if jump > u16::MAX.into() {
            return Err(CompileErr("Too much code to jump".to_string()));
        }
        self.current_mut_frame().function.chunk.opcodes[offset] = (jump as u16 >> 8) as u8;
        self.current_mut_frame().function.chunk.opcodes[offset + 1] = jump as u8;

        Ok(())
    }

    /// Emit a return opocode.
    fn emit_return(&mut self) {
        match self.current_frame().function_typ {
            CompileTarget::Script => self.emit_opcode(Opcode::Halt),
            CompileTarget::Function => {
                self.emit_opcode(Opcode::Unit);
                self.emit_opcode(Opcode::Return);
            }
            CompileTarget::Constructor => {
                self.emit_arg(Opcode::LoadLocal, 0);
                self.emit_opcode(Opcode::Return);
            }
            CompileTarget::Method => {
                //self.emit_arg(Opcode::LoadLocal, 0);
                //self.emit_opcode(Opcode::Del);
                self.emit_opcode(Opcode::Unit);
                self.emit_opcode(Opcode::Return);
            }
            CompileTarget::Module => {
                self.emit_opcode(Opcode::Halt);
            }
        }
    }

    /// Emit an opcode, followed by an 8-bit argument.
    fn emit_arg(&mut self, opcode: Opcode, arg: u8) {
        self.emit_opcode(opcode);
        self.emit_byte(arg);
    }

    /// Emit an opcode.
    fn emit_opcode(&mut self, opcode: Opcode) {
        let byte = opcode as u8;
        self.emit_byte(byte);
    }

    /// Emit a byte.
    fn emit_byte(&mut self, opcode: u8) {
        self.current_mut_frame().function.chunk.opcodes.push(opcode);
    }

    /// Compile an [AST] into a chunk of bytecode.
    pub fn run(&mut self, ast: &AST, globals: Scope) -> Result<Function, CompileErr> {
        self.globals = globals;

        let frame = Frame::script();
        self.frames.push(frame);

        //self.add_local("");

        for node in &ast.nodes {
            match node {
                ASTNode::Stmt(stmt) => self.statment(stmt)?,
                ASTNode::Expr(expr) => self.expression(expr)?,
            };
        }

        self.emit_return();

        let script = self.frames.pop().unwrap().function;

        Ok(script)
    }
}

impl Pass<(), CompileErr> for Compiler {
    /// Compile a statment.
    fn statment(&mut self, stmt: &Stmt) -> Result<(), CompileErr> {
        self.current_mut_frame()
            .function
            .chunk
            .emit_span(stmt.span());

        match stmt {
            Stmt::Block(stmts, _) => self.block(stmts),
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::ImportStatement(import, _) => self.import_statement(import),
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

    /// Compile an if-else statement.
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

    /// Compile a while statement.
    fn while_statement(&mut self, condition: &Expr, block: &Stmt) -> Result<(), CompileErr> {
        let loop_start = self.current_frame().function.chunk.opcodes.len();

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

    /// Compile a loop statment.
    fn loop_statement(&mut self, block: &Stmt) -> Result<(), CompileErr> {
        let start_ip = self.current_frame().function.chunk.opcodes.len();

        self.loop_stack.push(Loop::new(start_ip));

        self.statment(block)?;

        self.emit_loop(start_ip);
        self.leave_loop()?;

        Ok(())
    }

    /// Compile an import statement.
    fn import_statement(&mut self, import: &Expr) -> Result<(), CompileErr> {
        self.emit_opcode(Opcode::Import);
        self.expression(import)?;

        Ok(())
    }

    /// Compile a block.
    fn block(&mut self, block: &[Stmt]) -> Result<(), CompileErr> {
        self.enter_scope();

        for node in block {
            self.statment(node)?;
        }

        self.exit_scope();

        Ok(())
    }

    /// Compile a variable declaration.
    fn var_decl(&mut self, ident: &Ident, expr: &Option<Expr>) -> Result<(), CompileErr> {
        let expr = if let Some(expr) = expr {
            expr.clone()
        } else {
            Expr::Nil(Span::empty())
        };

        if self.current_frame().locals.depth > 0 {
            self.expression(&expr)?;
            self.add_local(&ident.name);
        } else {
            let global = self.emit_indent(ident.name.to_owned());

            self.expression(&expr)?;

            self.declare_global(global);
        }

        Ok(())
    }

    /// Compile a constant declaration.
    fn con_decl(&mut self, ident: &Ident, expr: &Expr) -> Result<(), CompileErr> {
        if self.current_frame().locals.depth > 0 {
            self.expression(expr)?;
            self.add_local(&ident.name);
        } else {
            let global = self.emit_indent(ident.name.to_owned());

            self.expression(expr)?;

            self.declare_global(global);
        }

        Ok(())
    }

    /// Compile an assignment statement.
    fn assign_stmt(&mut self, ident: &Expr, expr: &Expr) -> Result<(), CompileErr> {
        self.expression(expr)?;

        if let Expr::Identifier(name) = ident {
            self.save_variable(&name.name);
        }

        if let Expr::Index(expr, index, _) = ident {
            self.expression(expr)?;
            self.expression(index)?;

            self.emit_opcode(Opcode::SetIndex);

            if let Expr::Identifier(name) = &**expr {
                self.save_variable(&name.name);
            }
        }

        if let Expr::MemberExpr(obj, _prop, _) = ident {
            self.expression(obj)?;
        }

        Ok(())
    }

    /// Compile a class declaration.
    fn class(&mut self, class: &Class) -> Result<(), CompileErr> {
        let offset = self.emit_indent(class.name());

        self.emit_opcode(Opcode::Class);
        self.emit_byte(class.fields.len() as u8);
        self.emit_byte(offset as u8);

        self.declare_variable(&class.name());

        self.enter_scope();

        self.identifier(&class.name)?;

        for constructor in &class.constructors {
            if let Stmt::Constructor(constructor, _) = constructor {
                self.compile_function(
                    &constructor.name,
                    &constructor.params,
                    &constructor.body,
                    CompileTarget::Constructor,
                )?;

                let offset = self.emit_indent(constructor.name.name.to_owned());
                self.emit_arg(Opcode::Constructor, offset as u8);
            }
        }

        for method in &class.methods {
            if let Stmt::ScriptFun(fun, _) = method {
                self.compile_function(&fun.name, &fun.params, &fun.body, CompileTarget::Method)?;

                let offset = self.emit_indent(fun.name.name.to_owned());
                self.emit_arg(Opcode::Method, offset as u8);
            }
        }

        self.current_mut_frame().locals.depth -= 1;

        while self.current_frame().locals.locals_count > 0
            && self.current_frame().locals.locals[self.current_frame().locals.locals_count - 1]
                .depth
                > self.current_frame().locals.depth
        {
            self.current_mut_frame().locals.locals.pop();

            self.current_mut_frame().locals.locals_count -= 1;
        }

        self.emit_opcode(Opcode::Del);

        Ok(())
    }

    /// Compile a class constructor.
    fn constructor(&mut self, constructor: &Constructor) -> Result<(), CompileErr> {
        self.compile_function(
            &constructor.name,
            &constructor.params,
            &constructor.body,
            CompileTarget::Constructor,
        )?;

        self.identifier(&constructor.name)?;
        self.emit_opcode(Opcode::Constructor);

        Ok(())
    }

    /// Compile a function.
    fn fun(&mut self, fun: &ScriptFun) -> Result<(), CompileErr> {
        self.compile_function(&fun.name, &fun.params, &fun.body, CompileTarget::Function)
    }

    /// Compile a return statement.
    fn return_stmt(&mut self, expr: &Option<Expr>) -> Result<(), CompileErr> {
        if let Some(expr) = expr {
            self.expression(expr)?;
        } else {
            self.emit_opcode(Opcode::Unit);
        }
        self.emit_opcode(Opcode::Return);

        Ok(())
    }

    /// Compile a break statement.
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

    /// Compile a continue statement.
    fn continue_stmt(&mut self) -> Result<(), CompileErr> {
        let loop_start = self.current_loop()?.start_ip;

        self.emit_loop(loop_start);

        Ok(())
    }

    /// Compile an expression.
    fn expression(&mut self, expr: &Expr) -> Result<(), CompileErr> {
        self.current_mut_frame()
            .function
            .chunk
            .emit_span(expr.span());

        match expr {
            Expr::Number(val, _) => self.number(val),
            Expr::String(val, _) => self.string(val),
            Expr::Boolean(val, _) => self.boolean(val),
            Expr::Unit(_) | Expr::Nil(_) => self.nil(),
            Expr::Identifier(ident) => self.identifier(ident),
            Expr::SelfExpr(_) => self.self_expr(),
            Expr::BinExpr(bin_expr, _) => self.binary_expr(bin_expr),
            Expr::UnaryExpr(op, unary_expr, _) => self.unary_expr(op, unary_expr),
            Expr::ParenExpr(expr, _) => self.expression(&*expr),
            Expr::Index(expr, index, _) => self.index(expr, index),
            Expr::List(list, _) => self.list((list).to_vec()),
            Expr::Tuple(tuple, _) => self.tuple(tuple),
            Expr::Map(map, _) => self.map(map),
            Expr::Or(lhs, rhs, _) => self.or(lhs, rhs),
            Expr::And(lhs, rhs, _) => self.and(lhs, rhs),
            Expr::FunCall(callee, args, _) => self.fun_call(callee, args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(obj, prop),
            Expr::AssocExpr(obj, prop, _) => self.assoc_expr(obj, prop),
            Expr::Type(typ, _) => self.type_spec(typ),
        }
    }

    /// Compile type information.
    fn type_spec(&mut self, _typ: &TypePath) -> Result<(), CompileErr> {
        Ok(())
    }

    /// Compile an `and` expression.
    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), CompileErr> {
        self.expression(lhs)?;

        let jump_offset = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_opcode(Opcode::Del);
        self.expression(rhs)?;

        self.patch_jump(jump_offset)?;

        Ok(())
    }

    /// Compile an `or` expression.
    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), CompileErr> {
        self.expression(lhs)?;

        let jump_offset = self.emit_jump(Opcode::JumpIfTrue);
        self.emit_opcode(Opcode::Del);
        self.expression(rhs)?;

        self.patch_jump(jump_offset)?;

        Ok(())
    }

    /// Compile a binary expression.
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
            Op::BitwiseAnd => self.emit_opcode(Opcode::BitAnd),
            Op::BitwiseOr => self.emit_opcode(Opcode::BitOr),
            Op::BitwiseXor => self.emit_opcode(Opcode::BitXor),
            _ => {}
        }

        Ok(())
    }

    /// Compile a unary expression.
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

    /// Compile an index expression.
    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<(), CompileErr> {
        self.expression(expr)?;
        self.expression(index)?;

        self.emit_opcode(Opcode::GetIndex);

        Ok(())
    }

    /// Compile a tuple.
    fn tuple(&mut self, tuple: &[Expr]) -> Result<(), CompileErr> {
        for item in tuple.iter().rev() {
            self.expression(item)?;
        }

        self.emit_arg(Opcode::BuildTuple, tuple.len() as u8);

        Ok(())
    }

    /// Compile a list.
    fn list(&mut self, list: Vec<Expr>) -> Result<(), CompileErr> {
        for item in list.iter().rev() {
            self.expression(item)?;
        }
        self.emit_arg(Opcode::List, list.len() as u8);

        Ok(())
    }

    /// Compile a map.
    fn map(&mut self, map: &[(Expr, Expr)]) -> Result<(), CompileErr> {
        for (key, value) in map.iter().rev() {
            self.expression(value)?;
            if let Expr::Identifier(ident) = key {
                let index = self.emit_constant(Value::String(Rc::new(ident.name.clone())));
                self.emit_arg(Opcode::Const, index as u8);
            }
        }

        self.emit_arg(Opcode::BuildMap, map.len() as u8);

        Ok(())
    }

    /// Compile a function call.
    fn fun_call(&mut self, ident: &Expr, args: &[Expr]) -> Result<(), CompileErr> {
        self.expression(ident)?;

        for arg in args.iter().rev() {
            self.expression(arg)?;
        }

        /*if let Expr::MemberExpr(..) = ident {
            self.emit_arg(Opcode::Call, (args.len() + 1) as u8);
        } else {*/
            self.emit_arg(Opcode::Call, args.len() as u8);
        //}

        self.current_mut_frame()
            .function
            .chunk
            .emit_span(ident.span());

        Ok(())
    }

    /// Compile an associtive method.
    fn assoc_expr(&mut self, obj: &Expr, prop: &Expr) -> Result<(), CompileErr> {
        self.expression(obj)?;

        if let Expr::Identifier(id) = prop {
            let index = self.emit_constant(Value::String(Rc::new(id.name.to_owned())));
            self.emit_arg(Opcode::Get, index as u8);
        }

        Ok(())
    }

    /// Compile a member expression.
    fn member_expr(&mut self, object: &Expr, property: &Expr) -> Result<(), CompileErr> {
        self.expression(object)?;

        if let Expr::Identifier(id) = property {
            let index = self.emit_constant(Value::String(Rc::new(id.name.to_owned())));
            self.emit_arg(Opcode::Get, index as u8);
        }

        Ok(())
    }

    /// Compile `self`.
    fn self_expr(&mut self) -> Result<(), CompileErr> {
        self.emit_arg(Opcode::LoadUpValue, 0);

        Ok(())
    }

    /// Compile an identifer (e.g. variable or function name).
    fn identifier(&mut self, id: &Ident) -> Result<(), CompileErr> {
        if self.current_frame().locals.depth == 0 {
            let index = self.emit_indent(id.name.to_owned());
            self.emit_arg(Opcode::GetGlobal, index as u8);

            return Ok(());
        }

        let _ = match self.resolve_local(&id.name, self.current_frame()) {
            Some(index) => {
                self.emit_arg(Opcode::LoadLocal, index as u8);
            }
            None => match self.resolve_upvalue(&id.name) {
                Some(index) => {
                    self.emit_arg(Opcode::LoadUpValue, index as u8);
                }
                None => {
                    let index = self.emit_indent(id.name.to_owned());
                    self.emit_arg(Opcode::GetGlobal, index as u8);
                }
            },
        };

        Ok(())
    }

    /// Compile a number literal.
    fn number(&mut self, val: &f64) -> Result<(), CompileErr> {
        let offset = self.emit_constant(Value::Number(*val));
        self.emit_arg(Opcode::Const, offset as u8);

        Ok(())
    }

    /// Compile a string literal.
    fn string(&mut self, val: &str) -> Result<(), CompileErr> {
        let offset = self.emit_indent(val.to_string());
        self.emit_arg(Opcode::Const, offset as u8);

        Ok(())
    }

    /// Compile a boolean literal.
    fn boolean(&mut self, val: &bool) -> Result<(), CompileErr> {
        if *val {
            self.emit_opcode(Opcode::True);
        } else {
            self.emit_opcode(Opcode::False);
        }

        Ok(())
    }

    /// Compile a `nil` literal.
    fn nil(&mut self) -> Result<(), CompileErr> {
        self.emit_opcode(Opcode::Nil);

        Ok(())
    }
}
