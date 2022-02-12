use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{
    hash_map::Entry::{Occupied, Vacant},
    HashMap,
};
use std::mem;
use std::rc::Rc;
use std::u8;

use crate::common::{
    Captured, Closure, Function, KaonFile, NativeFun, Opcode, Upvalue, Value, ValueMap,
};
use crate::core::CoreLib;
use crate::vm::{Frame, KaonStderr, KaonStdin, KaonStdout, Slot, Stack, Trace};

pub struct VmSettings {
    pub stdout: Rc<dyn KaonFile>,
    pub stdin: Rc<dyn KaonFile>,
    pub stderr: Rc<dyn KaonFile>,
}

impl Default for VmSettings {
    fn default() -> Self {
        VmSettings {
            stdout: Rc::new(KaonStdout::default()),
            stdin: Rc::new(KaonStdin::default()),
            stderr: Rc::new(KaonStderr::default()),
        }
    }
}

pub struct VmContext {
    pub settings: VmSettings,
    pub globals: HashMap<String, Value>,
    pub prelude: ValueMap,
}

impl VmContext {
    pub fn with_settings(settings: VmSettings) -> Self {
        let core_lib = CoreLib::new();

        let mut prelude = ValueMap::new();
        prelude.insert_map("io", core_lib.io);
        prelude.insert_map("os", core_lib.os);
        prelude.insert_map("math", core_lib.math);
        prelude.insert_map("string", core_lib.string);
        prelude.insert_map("list", core_lib.list);
        prelude.insert_map("tuple", core_lib.tuple);

        VmContext {
            settings,
            globals: HashMap::new(),
            prelude,
        }
    }
}

impl Default for VmContext {
    fn default() -> Self {
        Self::with_settings(VmSettings::default())
    }
}

pub struct Vm {
    pub closure: Closure,
    pub stack: Stack,
    pub frames: Vec<Frame>,
    pub ip: usize,
    pub base_ip: usize,
    pub context: Rc<RefCell<VmContext>>,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            closure: Closure::empty(),
            frames: Vec::with_capacity(255),
            stack: Stack::new(),
            ip: 0,
            base_ip: 0,
            context: Rc::new(RefCell::new(VmContext::default())),
        }
    }

    pub fn with_settings(settings: VmSettings) -> Vm {
        Vm {
            closure: Closure::empty(),
            frames: Vec::with_capacity(255),
            stack: Stack::new(),
            ip: 0,
            base_ip: 0,
            context: Rc::new(RefCell::new(VmContext::with_settings(settings))),
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
            self.stack.debug_stack();

            match self.decode_opcode() {
                Opcode::Const => {
                    let index = self.next_number();
                    self.stack
                        .push_slot(self.closure.function.chunk.constants[index].clone());
                    self.next();
                }
                Opcode::True => {
                    self.stack.push_slot(Value::Boolean(true));
                }
                Opcode::False => {
                    self.stack.push_slot(Value::Boolean(false));
                }
                Opcode::Nil => {
                    self.stack.push_slot(Value::Nil);
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
                    self.stack.push(Slot::new(Value::Boolean(lhs == rhs)));
                }
                Opcode::NotEqual => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Value::Boolean(lhs != rhs)));
                }
                Opcode::Gte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Value::Boolean(lhs >= rhs)));
                }
                Opcode::Lte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Value::Boolean(lhs <= rhs)));
                }
                Opcode::Gt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Value::Boolean(lhs > rhs)));
                }
                Opcode::Lt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Slot::new(Value::Boolean(lhs < rhs)));
                }
                Opcode::Not => {
                    let val = self.stack.pop();
                    if let Value::Boolean(val) = val {
                        self.stack.push(Slot::new(Value::Boolean(!val)));
                    }
                }
                Opcode::DefGlobal => {
                    let name = self.get_constant().clone();
                    self.context
                        .as_ref()
                        .borrow_mut()
                        .globals
                        .insert(name.to_string(), self.stack.pop());

                    self.next();
                }
                Opcode::SetGlobal => {
                    let name = self.get_constant().clone();
                    match self
                        .context
                        .as_ref()
                        .borrow_mut()
                        .globals
                        .entry(name.to_string())
                    {
                        Occupied(mut val) => val.insert(self.stack.pop()),
                        Vacant(_) => panic!("Cannot assign to undefined variable"),
                    };

                    self.next();
                }
                Opcode::GetGlobal => {
                    let name = self.get_constant();
                    let context = &self.context.as_ref().borrow_mut();
                    match context.globals.get(&name.to_string()) {
                        Some(val) => self.stack.push(Slot::new(val.clone())),
                        None => match context.prelude.get(&name.to_string()) {
                            Some(val) => self.stack.push_slot(val.clone()),
                            None => panic!("cannot find '{}' in this scope", &name.to_string()),
                        },
                    }

                    self.ip += 1;
                }
                Opcode::SaveLocal => {
                    let data = self.stack.pop();

                    let index = self.next_number();
                    self.stack.save_local(index + self.base_ip, data);

                    self.next();
                }
                Opcode::LoadLocal => {
                    let index = self.next_number();
                    let slot = self.stack.get(index + self.base_ip);
                    self.stack.push(slot);

                    self.next();
                }
                Opcode::SaveUpValue => {
                    let index = self.next_number();
                    let data = self.stack.pop();
                    self.closure.captures[index].value = data;

                    self.next();
                }
                Opcode::LoadUpValue => {
                    let index = self.next_number();
                    let data = self.closure.captures[index].borrow().to_owned();

                    self.stack.push_slot(data.value);

                    self.next();
                }
                Opcode::CloseUpValue => {
                    self.next();
                }
                Opcode::Loop => {
                    self.ip -= self.read_short();
                }
                Opcode::Jump => {
                    self.ip += self.read_short();
                }
                Opcode::JumpIfFalse => {
                    let base_ip = self.read_short();
                    if self.is_falsy() {
                        self.ip += base_ip;
                    }
                }
                Opcode::JumpIfTrue => {
                    let base_ip = self.read_short();
                    if !self.is_falsy() {
                        self.ip += base_ip;
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
                Opcode::BuildTuple => self.tuple()?,
                Opcode::BuildMap => self.map()?,
                Opcode::Index => self.index()?,
                Opcode::Get => self.map_get()?,
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
            Value::Function(fun) => fun,
            _ => panic!("expected a function"),
        };

        let mut closure = Closure::wrap(Rc::new(fun.clone()));

        for captured in closure.function.captures.iter() {
            let reference = match captured {
                Captured::Local(index) => self.capture_upvalue(*index),
                Captured::NonLocal(index) => self.closure.captures[*index].clone(),
            };

            closure.captures.push(reference);
        }

        self.stack.push_slot(Value::Closure(closure));
        self.done()
    }

    fn call(&mut self) -> Result<(), Trace> {
        let arity = self.next_number();
        self.ip += 1;

        match self.stack.pop() {
            Value::NativeFun(fun) => {
                self.ffi_call(*fun, arity);
            }
            Value::Closure(closure) => {
                self.fun_call(closure);
            }
            _ => {
                return Err(Trace::new("can only call functions", self.frames.clone()));
            }
        }
        Ok(())
    }

    fn ffi_call(&mut self, fun: NativeFun, arity: usize) {
        let mut args = vec![];
        for _ in 0..arity {
            args.push(self.stack.pop());
        }

        let result = fun.fun.0(Rc::clone(&self.context), args);
        self.stack.push(Slot::new(result));
    }

    fn fun_call(&mut self, closure: Closure) {
        let mut old_closure = mem::replace(&mut self.closure, closure);

        let old_ip = mem::replace(&mut self.ip, 0);

        let base_ip = mem::replace(
            &mut self.base_ip,
            self.stack.stack.len() - self.closure.function.arity,
        );

        let suspend = Frame::new(&mut old_closure, old_ip, base_ip);
        self.frames.push(suspend);
    }

    fn return_(&mut self) {
        let return_val = self.stack.pop();

        self.next();

        self.stack.stack.truncate(self.base_ip);

        let suspend = self.frames.pop().unwrap();

        self.ip = suspend.ip;
        self.closure = suspend.closure;
        self.base_ip = suspend.offset;

        self.stack.push(Slot::new(return_val));
    }

    fn capture_upvalue(&mut self, index: usize) -> Upvalue {
        let new_upvalue =
            Upvalue::new(self.base_ip + index, self.stack.get(self.base_ip + index).0);

        return new_upvalue;
    }

    fn list(&mut self) -> Result<(), Trace> {
        let mut list: Vec<Value> = vec![];
        let length = self.get_opcode(self.ip) as usize;
        for _ in 0..length {
            list.push(self.stack.pop());
        }
        self.stack.push(Slot::new(Value::List(list)));

        self.done()
    }

    fn tuple(&mut self) -> Result<(), Trace> {
        let mut tuple = vec![];
        let length = self.get_opcode(self.ip) as usize;
        for _ in 0..length {
            tuple.push(self.stack.pop());
        }
        self.stack.push(Slot::new(Value::Tuple(tuple)));

        self.done()
    }

    fn map(&mut self) -> Result<(), Trace> {
        let mut map = ValueMap::new();
        let length = self.get_opcode(self.ip) as usize;
        for _ in 0..length {
            let key = self.stack.pop();
            let value = self.stack.pop();
            map.insert_constant(&key.to_string(), value);
        }
        self.stack.push_slot(Value::Map(map));

        self.done()
    }

    fn index(&mut self) -> Result<(), Trace> {
        let index = self.stack.pop();
        let expr = self.stack.pop();

        match index {
            Value::Number(index) => {
                self.stack.push_slot(expr[index].clone());
                return Ok(());
            }
            _ => return Err(Trace::new("can only index into lists", self.frames.clone())),
        }
    }

    fn map_get(&mut self) -> Result<(), Trace> {
        match self.stack.pop() {
            Value::Map(map) => {
                self.stack.push_slot(
                    map.get(&self.get_constant().to_string()[..])
                        .unwrap()
                        .clone(),
                );
            }
            Value::String(val) => {
                self.stack.push_slot(Value::String(val));
                if let Value::Map(map) = self
                    .context
                    .as_ref()
                    .borrow_mut()
                    .prelude
                    .get("string")
                    .unwrap()
                {
                    self.stack.push_slot(
                        map.get(&self.get_constant().to_string()[..])
                            .unwrap()
                            .clone(),
                    );
                }
            }
            Value::List(list) => {
                self.stack.push_slot(Value::List(list));
                if let Value::Map(map) = self
                    .context
                    .as_ref()
                    .borrow_mut()
                    .prelude
                    .get("list")
                    .unwrap()
                {
                    self.stack.push_slot(
                        map.get(&self.get_constant().to_string()[..])
                            .unwrap()
                            .clone(),
                    );
                }
            }
            Value::Tuple(tuple) => {
                self.stack.push_slot(Value::Tuple(tuple));
                if let Value::Map(map) = self
                    .context
                    .as_ref()
                    .borrow_mut()
                    .prelude
                    .get("tuple")
                    .unwrap()
                {
                    self.stack.push_slot(
                        map.get(&self.get_constant().to_string()[..])
                            .unwrap()
                            .clone(),
                    );
                }
            }
            Value::External(external) => {
                self.stack.push_slot(Value::External(external.clone()));
                self.stack.push_slot(
                    external
                        .meta_map
                        .as_ref()
                        .borrow_mut()
                        .get(&self.get_constant().to_string()[..]),
                )
            }
            _ => return Err(Trace::new("can only index into a map", self.frames.clone())),
        }

        self.done()
    }

    fn read_short(&mut self) -> usize {
        self.ip += 2;
        let base_ip = ((self.closure.function.chunk.opcodes[self.ip - 2] as u16) << 8)
            | self.closure.function.chunk.opcodes[self.ip - 1] as u16;
        return base_ip as usize;
    }

    fn is_falsy(&mut self) -> bool {
        matches!(self.stack.peek(), Value::Boolean(false))
    }

    #[inline]
    fn next(&mut self) {
        self.ip += 1;
    }

    #[inline]
    fn done(&mut self) -> Result<(), Trace> {
        self.ip += 1;
        Ok(())
    }

    #[inline]
    fn get_opcode(&mut self, index: usize) -> u8 {
        self.closure.function.chunk.opcodes[index]
    }

    #[inline]
    fn get_constant(&self) -> &Value {
        &self.closure.function.chunk.constants[self.next_number()]
    }

    #[inline]
    fn decode_opcode(&mut self) -> Opcode {
        self.next();
        Opcode::from(self.closure.function.chunk.opcodes[self.ip - 1])
    }

    pub fn prelude(&self) -> ValueMap {
        self.context.as_ref().borrow_mut().prelude.clone()
    }
}
