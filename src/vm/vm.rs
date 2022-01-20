use std::collections::{
    hash_map::Entry::{Occupied, Vacant},
    HashMap,
};

use crate::common::{ByteCode, Data, Function, Opcode};
use crate::vm::{Slot, Stack};

#[allow(dead_code)]
pub struct Frame {
    function: Function,
    ip: usize,
    slots: Vec<Data>,
}

impl Frame {
    pub fn new(function: Function) -> Self {
        Frame {
            function,
            ip: 0,
            slots: Vec::new(),
        }
    }
}

pub struct Vm {
    pub chunk: ByteCode,
    pub stack: Stack,
    pub frames: Vec<Frame>,
    pub ip: usize,
    pub globals: HashMap<String, Data>,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            chunk: ByteCode::empty(),
            frames: Vec::new(),
            stack: Stack::new(),
            ip: 0,
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, chunk: ByteCode) {
        self.chunk = chunk;
        self.run();
    }

    pub fn run(&mut self) {
        loop {
            match self.decode_opcode() {
                Opcode::Const => {
                    let index = self.chunk.opcodes[self.ip];
                    self.stack
                        .push(Slot::new(self.chunk.constants[(index) as usize].clone()));
                    self.next();
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
                Opcode::DefGlobal => {
                    let name = self.get_constant().clone();
                    self.globals.insert(name.to_string(), self.stack.pop());

                    self.next();
                }
                Opcode::SetGlobal => {
                    let name = self.get_constant().clone();
                    let entry = self.globals.entry(name.to_string());
                    match entry {
                        Occupied(mut val) => val.insert(self.stack.pop()),
                        Vacant(_) => panic!("Cannot assign to undefined variable"),
                    };

                    self.next();
                }
                Opcode::GetGlobal => {
                    let name = self.get_constant(); //.clone();
                    match self.globals.get(&name.to_string()) {
                        Some(val) => self.stack.push(Slot::new(val.clone())),
                        None => panic!("Found undefined variable"),
                    }

                    self.next();
                }
                Opcode::SaveLocal => {
                    let data = self.stack.pop();

                    let index = self.chunk.opcodes[self.ip] as usize;
                    self.stack.save_local(index, data);

                    self.next();
                }
                Opcode::LoadLocal => {
                    let index = self.chunk.opcodes[self.ip] as usize;
                    let slot = self.stack.get(index);
                    self.stack.push(slot);

                    self.next();
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
                Opcode::Call => {
                    let offset = self.chunk.opcodes[self.ip] as usize;
                    let fun = self.chunk.constants[offset].clone();
                    let mut args = vec![];
                    if let Data::NativeFun(fun) = fun {
                        for _ in 0..fun.arity {
                            args.push(self.stack.pop());
                        }

                        let result = fun.fun.0(args);
                        self.stack.push(Slot::new(result));
                    }

                    self.next();
                }
                Opcode::List => {
                    let mut list: Vec<Data> = vec![];
                    let length = self.get_opcode(self.ip) as usize;
                    for _ in 0..length {
                        list.push(self.stack.pop());
                    }
                    self.stack.push(Slot::new(Data::List(list)));
                    self.next();
                }
                Opcode::Loop => {
                    let offset = ((self.chunk.opcodes[self.ip] as u16) << 8)
                        | self.chunk.opcodes[self.ip + 1] as u16;
                    self.ip -= offset as usize - 2;
                }
                Opcode::Del => {
                    self.stack.pop();
                }
                Opcode::Halt => {
                    break;
                }
            }
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

    fn next(&mut self) {
        self.ip += 1;
    }

    fn get_opcode(&mut self, index: usize) -> u8 {
        self.chunk.opcodes[index]
    }

    fn get_constant(&self) -> &Data {
        &self.chunk.constants[self.chunk.opcodes[self.ip] as usize]
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.chunk.opcodes[self.ip]);
        self.ip += 1;
        return op;
    }
}
