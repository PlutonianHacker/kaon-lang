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
            chunk: ByteCode { opcodes: vec![], constants: vec![] },
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
                    //println!("{}", self.pos);
                    self.stack.push(self.chunk.constants[self.chunk.opcodes[self.pos - 1] as usize]);
                    //println!("{:?}", self.stack);
                }
                Opcode::Add => {
                    let lhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let rhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let res = lhs + rhs;
                    println!("{}", res);
                    self.stack.push(res);
                }
                Opcode::Sub => {
                    let lhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let rhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let res = lhs - rhs;
                    println!("{}", res);
                    self.stack.push(res);
                }
                Opcode::Mul => {
                    let lhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let rhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let res = lhs * rhs;
                    println!("{}", res);
                    self.stack.push(res);
                }
                Opcode::Div => {
                    let lhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let rhs = self.stack.pop().unwrap();//[self.stack.pop().unwrap() as usize];
                    let res = lhs / rhs;
                    println!("{}", res);
                    self.stack.push(res);
                }
                Opcode::Halt => {
                    //println!("{:?}", self.stack[0]);
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