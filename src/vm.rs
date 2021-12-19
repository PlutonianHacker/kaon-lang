use crate::opcode::ByteCode;
use crate::opcode::Opcode;

pub struct Vm {
    chunk: ByteCode,
    stack: Vec<f64>,
    pos: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            chunk: ByteCode {
                opcodes: vec![],
                constants: vec![],
            },
            stack: vec![],
            pos: 0,
        }
    }

    pub fn run(&mut self, code: ByteCode) {
        self.chunk = code;
        loop {
            match self.decode_opcode() {
                Opcode::Const => {
                    self.pos += 1;
                    self.stack
                        .push(self.chunk.constants[self.chunk.opcodes[self.pos - 1] as usize]);
                }
                Opcode::Add => {
                    let lhs = self.stack.pop().unwrap();
                    let rhs = self.stack.pop().unwrap();
                    let res = lhs + rhs;
                    self.stack.push(res);
                }
                Opcode::Sub => {
                    let lhs = self.stack.pop().unwrap();
                    let rhs = self.stack.pop().unwrap();
                    let res = lhs - rhs;
                    self.stack.push(res);
                }
                Opcode::Mul => {
                    let lhs = self.stack.pop().unwrap();
                    let rhs = self.stack.pop().unwrap();
                    let res = lhs * rhs;
                    self.stack.push(res);
                }
                Opcode::Div => {
                    let lhs = self.stack.pop().unwrap();
                    let rhs = self.stack.pop().unwrap();
                    let res = lhs / rhs;
                    self.stack.push(res);
                }
                Opcode::Negate => {
                    let val = self.stack.pop().unwrap();
                    let res = -val;
                    self.stack.push(res);
                }
                Opcode::Halt => {
                    println!("{:?}", self.stack[self.stack.len() - 1]);
                    break;
                }
            }
        }
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.chunk.opcodes[self.pos]);
        self.pos += 1;
        return op;
    }
}
