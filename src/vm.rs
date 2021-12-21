use std::collections::HashMap;

use crate::opcode::ByteCode;
use crate::opcode::Opcode;
use crate::stack::Data;
use crate::stack::Slot;
use crate::stack::Stack;

pub struct Vm {
    chunk: ByteCode,
    pub stack: Stack,
    ip: usize,
    globals: HashMap<String, Data>,
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
            globals: HashMap::new(),
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
                Opcode::Gte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Number(lhs), Data::Number(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs >= rhs)));
                    }
                }
                Opcode::Lte => {
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
                Opcode::Save => {
                    self.save();
                    self.get_next_opcode();
                }
                Opcode::Load => {
                    self.load();
                    self.get_next_opcode();
                }
                Opcode::Halt => {
                    break;
                }
            }
        }
    }

    fn save(&mut self) {
        let global_val = self.chunk.constants[self.chunk.opcodes[self.ip] as usize].clone();
        if let Data::String(id) = global_val {
            let val = self.stack.pop();
            self.globals.insert(id, val.clone());
            self.stack.push(Slot::new(val));
        }
    }

    fn load(&mut self) {
        let val = self.chunk.constants[self.chunk.opcodes[self.ip] as usize].clone();
        if let Data::String(id) = val {
            self.stack
                .push(Slot::new(self.globals.get(&id).unwrap().clone()));
        }
    }

    fn get_next_opcode(&mut self) {
        self.ip += 1;
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.chunk.opcodes[self.ip]);
        self.ip += 1;
        return op;
    }
}
