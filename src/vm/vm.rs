use std::borrow::Borrow;
use std::collections::{
    hash_map::Entry::{Occupied, Vacant},
    HashMap,
};
use std::mem;
use std::rc::Rc;
use std::u8;

use crate::common::{Captured, Closure, Data, Function, NativeFun, Opcode};
use crate::vm::{Frame, Slot, Stack, Trace};

pub struct Vm {
    pub closure: Closure,
    pub stack: Stack,
    pub frames: Vec<Frame>,
    pub ip: usize,
    pub globals: HashMap<String, Data>,

    pub offset: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            closure: Closure::empty(),
            frames: Vec::with_capacity(255),
            stack: Stack::new(),
            ip: 0,
            globals: HashMap::new(),

            offset: 0,
        }
    }

    pub fn interpret(&mut self, fun: Rc<Function>) {
        self.closure.function = fun;
        match self.run() {
            Err(traceback) => println!("{}", traceback),
            Ok(_) => {}
        }
    }

    fn next_number(&self) -> usize {
        self.closure.function.chunk.opcodes[self.ip] as usize
    }

    pub fn run(&mut self) -> Result<(), Trace> {
        loop {
            //self.stack.debug_stack();

            match self.decode_opcode() {
                Opcode::Const => {
                    let index = self.next_number();
                    self.stack
                        .push_slot(self.closure.function.chunk.constants[index].clone());
                    self.next();
                }
                Opcode::True => {
                    self.stack.push_slot(Data::Boolean(true));
                }
                Opcode::False => {
                    self.stack.push_slot(Data::Boolean(false));
                }
                Opcode::Nil => {
                    self.stack.push_slot(Data::Unit);
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
                    let name = self.get_constant();
                    match self.globals.get(&name.to_string()) {
                        Some(val) => self.stack.push(Slot::new(val.clone())),
                        None => panic!("Found undefined variable"),
                    }

                    self.next();
                }
                Opcode::SaveLocal => {
                    let data = self.stack.pop();

                    let index = self.next_number();
                    self.stack.save_local(index + self.offset, data);

                    self.next();
                }
                Opcode::LoadLocal => {
                    let index = self.next_number();
                    let slot = self.stack.get(index + self.offset);
                    self.stack.push(slot);

                    self.next();
                }
                Opcode::SaveUpValue => {
                    let index = self.next_number();
                    let data = self.stack.pop();
                    self.closure.captures[index] = data;

                    self.next();
                }
                Opcode::LoadUpValue => {
                    let index = self.next_number();
                    let data = self.closure.captures[index].borrow().to_owned();
                    self.stack.push_slot(data);

                    self.next();
                }
                Opcode::CloseUpValue => {}
                Opcode::Loop => {
                    self.ip -= self.read_short();
                }
                Opcode::Jump => {
                    self.ip += self.read_short();
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.is_falsy() {
                        self.ip += offset;
                    }
                }
                Opcode::JumpIfTrue => {
                    let offset = self.read_short();
                    if !self.is_falsy() {
                        self.ip += offset;
                    }
                }
                Opcode::Print => {
                    let expr = self.stack.pop();
                    println!("{}", expr);
                }
                Opcode::Call => self.call()?,
                Opcode::Closure => self.closure()?,
                Opcode::Return => self.return_(),
                Opcode::List => self.list()?,
                Opcode::Index => self.index()?,
                Opcode::Del => {
                    self.stack.pop();
                }
                Opcode::Halt => break,
            }
        }
        Ok(())
    }

    fn closure(&mut self) -> Result<(), Trace> {
        let fun = match self.get_constant() {
            Data::Function(fun) => fun,
            _ => panic!("expected a function"),
        };

        let mut closure = Closure::wrap(Rc::new(fun.clone()));

        for captured in closure.function.captures.iter() {
            let reference = match captured {
                Captured::Local(index) => self.stack.get(self.offset + index).0.clone(),
                Captured::NonLocal(index) => self.closure.captures[*index].clone(),
            };

            closure.captures.push(reference);
        }

        self.stack.push_slot(Data::Closure(closure));
        self.done()
    }

    fn call(&mut self) -> Result<(), Trace> {
        match self.stack.pop() {
            Data::NativeFun(fun) => {
                self.ffi_call(*fun);
            }
            Data::Function(_) => {
                //self.fun_call(fun);
            }
            Data::Closure(closure) => {
                self.fun_call(closure);
            }
            _ => {
                //let new_frame = Frame::new(&mut self.closure.function, self.ip, self.offset);
                //let mut frames = self.frames.clone();
                //frames.append(&mut vec![new_frame]);
                return Err(Trace::new("can only call functions", self.frames.clone()));
            }
        }
        Ok(())
    }

    fn ffi_call(&mut self, fun: NativeFun) {
        let mut args = vec![];
        for _ in 0..fun.arity {
            args.push(self.stack.pop());
        }

        let result = fun.fun.0(args);
        self.stack.push(Slot::new(result));
    }

    fn fun_call(&mut self, closure: Closure) {
        let mut old_closure = mem::replace(&mut self.closure, closure);

        let old_ip = mem::replace(&mut self.ip, 0);

        let offset = mem::replace(
            &mut self.offset,
            self.stack.stack.len() - self.closure.function.arity,
        );

        let suspend = Frame::new(&mut old_closure, old_ip, offset);
        self.frames.push(suspend);
    }

    fn return_(&mut self) {
        let return_val = self.stack.pop();

        self.next();

        self.stack.stack.truncate(self.offset);

        let suspend = self.frames.pop().unwrap();

        self.ip = suspend.ip;
        self.closure = suspend.closure;
        self.offset = suspend.offset;

        self.stack.push(Slot::new(return_val));
    }

    fn list(&mut self) -> Result<(), Trace> {
        let mut list: Vec<Data> = vec![];
        let length = self.get_opcode(self.ip) as usize;
        for _ in 0..length {
            list.push(self.stack.pop());
        }
        self.stack.push(Slot::new(Data::List(list)));

        self.done()
    }

    fn index(&mut self) -> Result<(), Trace> {
        let index = self.stack.pop();
        let expr = self.stack.pop();

        match index {
            Data::Number(index) => {
                self.stack.push_slot(expr[index].clone());
                return Ok(());
            }
            _ => return Err(Trace::new("can only index into lists", self.frames.clone())),
        }
    }

    fn read_short(&mut self) -> usize {
        self.ip += 2;
        let offset = ((self.closure.function.chunk.opcodes[self.ip - 2] as u16) << 8)
            | self.closure.function.chunk.opcodes[self.ip - 1] as u16;
        return offset as usize;
    }

    fn is_falsy(&mut self) -> bool {
        match self.stack.peek() {
            Data::Boolean(false) => true,
            _ => false,
        }
    }

    fn next(&mut self) {
        self.ip += 1;
    }

    fn done(&mut self) -> Result<(), Trace> {
        self.ip += 1;
        Ok(())
    }

    fn get_opcode(&mut self, index: usize) -> u8 {
        self.closure.function.chunk.opcodes[index]
    }

    fn get_constant(&self) -> &Data {
        &self.closure.function.chunk.constants[self.next_number()]
    }

    fn decode_opcode(&mut self) -> Opcode {
        let op = Opcode::from(self.closure.function.chunk.opcodes[self.ip]);
        self.next();
        return op;
    }
}
