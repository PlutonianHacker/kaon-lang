use crate::ast::ArgList;
use crate::ast::AST;
use crate::data::Data;
use crate::opcode::Opcode;

#[derive(Clone)]
pub struct Chunk {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Data>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            opcodes: vec![],
            constants: vec![],
        }
    }
}

pub struct Compiler {
    code: Chunk,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { code: Chunk::new() }
    }

    pub fn compile(&mut self, ast: Vec<AST>) -> &Chunk {
        for node in &ast {
            self.visit(node);
        }

        self.emit_opcode(Opcode::Halt);

        return &self.code;
    }

    pub fn visit(&mut self, node: &AST) {
        match node {
            AST::FuncCall(ident, args) => self.func_call(ident, args),
            AST::Number(val) => self.number(val),
            _ => todo!(),
        }
    }

    fn func_call(&mut self, ident: &str, args: &ArgList) {
        for arg in args {
            self.visit(&arg);
        }
        // ...
        let offset = self.code.constants.len() as u8;
        self.emit_opcode(Opcode::Call);
        self.code.constants.push(Data::String(ident.to_string()));
        self.emit_byte(offset);
    }

    fn number(&mut self, val: &f64) {
        let idx = self.code.constants.len() as u8;
        self.code.constants.push(Data::Number(*val));
        self.emit_opcode(Opcode::Const);
        self.emit_byte(idx);
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        self.code.opcodes.push(opcode as u8);
    }

    fn emit_byte(&mut self, byte: u8) {
        self.code.opcodes.push(byte);
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::Compiler;
    use crate::ast::AST;
    use crate::data::Data;

    #[test]
    fn test_compiler() {
        let mut compiler = Compiler::new();
        let ast = vec![AST::func_call("add", vec![AST::number(1.0), AST::number(2.0)])];
        let chunk = compiler.compile(ast);
        assert_eq!(chunk.opcodes, vec![1, 0, 1, 1, 2, 2, 0]);
        assert_eq!(chunk.constants, vec![Data::Number(1.0), Data::Number(2.0), Data::String("add".to_string())])
    }
}