use crate::opcode::ByteCode;
use crate::opcode::Opcode;
use crate::stack::Data;
use crate::stack::Slot;
use crate::stack::Stack;

pub struct Vm {
    chunk: ByteCode,
    pub stack: Stack,
    ip: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            chunk: ByteCode {
                opcodes: vec![],
                constants: vec![],
            },
            stack: Stack::new(),
            ip: 0,
        }
    }

    pub fn run(&mut self, code: ByteCode) {
        self.chunk = code;
        loop {
            match self.decode_opcode() {
                Opcode::Const => {
                    self.ip += 1;
                    self.stack.push(Slot::new(
                        self.chunk.constants[self.chunk.opcodes[self.ip - 1] as usize].clone(),
                    ));
                }
                Opcode::Add => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Number(lhs + rhs)));
                    }
                }
                Opcode::Sub => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Number(lhs - rhs)));
                    }
                }
                Opcode::Mul => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Number(lhs * rhs)));
                    }
                }
                Opcode::Div => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Number(lhs / rhs)));
                    }
                }
                Opcode::Negate => {
                    let val = self.stack.pop();
                    if let Data::Number(val) = val {
                        self.stack.push(Slot::new(Data::Number(-val)));
                    }
                }
                Opcode::Equal => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (&lhs, &rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs == rhs)));
                    }
                    if let (Data::Boolean(lhs), Data::Boolean(rhs)) = (&lhs, &rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs == rhs)));
                    }
                }
                Opcode::NotEqual => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (&lhs, &rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs != rhs)));
                    }
                    if let (Data::Boolean(lhs), Data::Boolean(rhs)) = (&lhs, &rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs != rhs)));
                    }
                }
                Opcode::GToEq => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs >= rhs)));
                    }
                }
                Opcode::LToEq => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs <= rhs)));
                    }
                }
                Opcode::Gt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs > rhs)));
                    }
                }
                Opcode::Lt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs < rhs)));
                    }
                }
                Opcode::Not => {
                    let val = self.stack.pop();
                    if let Data::Boolean(val) = val {
                        self.stack.push(Slot::new(Data::Boolean(!val)));
                    }
                }
                Opcode::SetGlobal => {}
                Opcode::Halt => {
                    break;
                }
            }
        }
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.chunk.opcodes[self.ip]);
        self.ip += 1;
        return op;
    }
}
