use std::collections::HashMap;

use crate::core::{ffi_core, FFI};
use crate::data::Data;
use crate::opcode::ByteCode;
use crate::opcode::Opcode;
use crate::stack::Slot;
use crate::stack::Stack;

pub struct Vm {
    pub chunk: ByteCode,
    pub stack: Stack,
    pub ip: usize,
    globals: HashMap<String, Data>,
    ffi: FFI,
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
            ffi: ffi_core(),
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
                    self.stack.push(Slot::new(lhs + rhs));
                }
                Opcode::Sub => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(lhs - rhs));
                }
                Opcode::Mul => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(lhs * rhs));
                }
                Opcode::Div => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(lhs / rhs));
                }
                Opcode::Mod => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(lhs % rhs));
                }
                Opcode::Negate => {
                    let val = self.stack.pop();
                    self.stack.push(Slot::new(-val));
                }
                Opcode::Equal => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Data::Boolean(lhs == rhs)));
                }
                Opcode::NotEqual => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Data::Boolean(lhs != rhs)));
                }
                Opcode::Gte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Data::Boolean(lhs >= rhs)));
                }
                Opcode::Lte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Data::Boolean(lhs <= rhs)));
                }
                Opcode::Gt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Data::Boolean(lhs > rhs)));
                }
                Opcode::Lt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Data::Boolean(lhs < rhs)));
                }
                Opcode::Not => {
                    let val = self.stack.pop();
                    if let Data::Boolean(val) = val {
                        self.stack.push(Slot::new(Data::Boolean(!val)));
                    }
                }
                Opcode::Or => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Boolean(lhs), Data::Boolean(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs || rhs)));
                    }
                }
                Opcode::And => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    if let (Data::Boolean(lhs), Data::Boolean(rhs)) = (lhs, rhs) {
                        self.stack.push(Slot::new(Data::Boolean(lhs && rhs)));
                    }
                }
                Opcode::SaveGlobal => {
                    self.save();
                    self.get_next_opcode();
                }
                Opcode::LoadGlobal => {
                    self.load();
                    self.get_next_opcode();
                }
                Opcode::SaveLocal => {
                    //println!("{:?}", self.stack);
                    let data = self.stack.pop();

                    let index = self.chunk.opcodes[self.ip] as usize;
                    self.stack.save_local(index, data);

                    //println!("{:?}", self.stack);

                    self.get_next_opcode();
                }
                Opcode::LoadLocal => {
                    let index = self.chunk.opcodes[self.ip] as usize;
                    let slot = self.stack.get(index);
                    self.stack.push(slot);

                    //println!("{:?}", self.stack);

                    self.get_next_opcode();
                    /*let slot = self.chunk.opcodes[self.ip];
                    self.get_next_opcode();
                    println!("{:?}", &slot);
                    self.stack
                        .push(Slot::new(self.chunk.constants[slot as usize].clone()));*/
                }
                Opcode::Jump => {
                    self.jump();
                }
                Opcode::Jeq => {
                    self.jump_if_not_eq();
                }
                Opcode::Print => {
                    let expr = self.stack.pop();
                    println!("{}", expr);
                }
                Opcode::FFICall => {
                    let offset = self.chunk.opcodes[self.ip] as usize;
                    let arg = self.stack.pop();
                    let ident = &self.chunk.constants[offset];
                    if let &Data::String(ref ident) = ident {
                        let fun = self.ffi.get(ident).unwrap();
                        fun.0(vec![arg]);
                        //self.stack.push(Slot::new(fun.0(vec![arg])));
                    }
                    self.get_next_opcode();
                }
                Opcode::List => {
                    let mut list: Vec<Data> = vec![];
                    let length = self.get_opcode(self.ip) as usize;
                    for _ in 0..length {
                        list.push(self.stack.pop());
                    }
                    
                    self.stack.push(Slot::new(Data::List(list)));
                    
                    self.get_next_opcode();
                }
                Opcode::Del => {
                    //println!("{:?}", self.stack);
                    self.stack.pop();
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

    fn jump(&mut self) {
        self.ip += 2;
        let offset = ((self.chunk.opcodes[self.ip - 2] as u16) << 8)
            | self.chunk.opcodes[self.ip - 1] as u16;
        self.ip += offset as usize;
    }

    fn jump_if_not_eq(&mut self) {
        if let Data::Boolean(condition) = self.stack.pop() {
            self.ip += 2;
            let offset = ((self.chunk.opcodes[self.ip - 2] as u16) << 8)
                | self.chunk.opcodes[self.ip - 1] as u16;
            if !condition {
                self.ip += offset as usize;
            }
        }
    }

    fn get_next_opcode(&mut self) {
        self.ip += 1;
    }

    fn get_opcode(&mut self, index: usize) -> u8 {
        self.chunk.opcodes[index]
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.chunk.opcodes[self.ip]);
        self.ip += 1;
        return op;
    }
}
