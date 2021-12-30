use crate::ffi;
use crate::compiler::Chunk;
use crate::data::Data;
use crate::opcode::Opcode;
use crate::stack::Stack;

pub struct Vm {
    ip: usize,
    pub stack: Stack<Data>,
    code: Chunk,
    ffi: ffi::FFI,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            ip: 0,
            stack: Stack::new(),
            code: Chunk::new(),
            ffi: ffi::core(),
        }
    }

    pub fn run(&mut self, chunk: Chunk) {
        self.code = chunk;
        loop {
            match self.next_opcode() {
                Opcode::Const => {
                    let offset = self.code.opcodes[self.ip] as usize;
                    self.ip += 1;
                    self.stack.push(self.code.constants[offset].clone());
                }
                Opcode::Call => {
                    let arg1 = self.stack.pop().unwrap();
                    let arg2 = self.stack.pop().unwrap();
                    let name = &self.code.constants[self.code.opcodes[self.ip] as usize];
                    self.ip += 1;
                    if let Data::String(val) = name {
                        let fun = self.ffi.get(val);
                        self.stack.push(fun.0(vec![arg2, arg1]));
                    }
                }
                Opcode::Halt => {
                    break
                }
            }
        }
    }

    fn next_opcode(&mut self) -> Opcode {
        let opcode = Opcode::from(self.code.opcodes[self.ip]);
        self.ip += 1;
        return opcode;
    }
}