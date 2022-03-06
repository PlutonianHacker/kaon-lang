use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::rc::Rc;
use std::u8;

use fnv::FnvHashMap;

use crate::common::value::{ValueList, ValueTuple};
use crate::common::{
    Captured, Closure, Constructor, Function, Instance, KaonFile, Loader, NativeFun, Opcode,
    Upvalue, Value, ValueMap,
};
use crate::core::CoreLib;
use crate::runtime::{Frame, KaonStderr, KaonStdin, KaonStdout, Stack, Trace};

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
    pub globals: FnvHashMap<String, Value>,
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
            globals: FnvHashMap::default(),
            prelude,
        }
    }
}

impl Default for VmContext {
    fn default() -> Self {
        Self::with_settings(VmSettings::default())
    }
}

/// The Kaon VM.
pub struct Vm {
    /// the operand stack
    pub stack: Stack,
    /// the callstack
    pub frames: Vec<Frame>,
    /// [Vm] context for managing state
    pub context: Rc<RefCell<VmContext>>,
    /// the number of frames on the call stack
    frame_count: usize,
    open_upvalues: Option<Upvalue>,
    /// module loader
    pub loader: Loader,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            frames: Vec::with_capacity(255),
            stack: Stack::new(),
            context: Rc::new(RefCell::new(VmContext::default())),
            frame_count: 0,
            loader: Loader::new(),
            open_upvalues: None,
        }
    }

    pub fn with_settings(settings: VmSettings) -> Vm {
        Vm {
            context: Rc::new(RefCell::new(VmContext::with_settings(settings))),
            ..Vm::default()
        }
    }

    pub fn interpret(&mut self, fun: Rc<Function>) -> Result<Value, String> {
        self.frames
            .push(Frame::new(Rc::new(RefCell::new(Closure::wrap(fun))), 0, 0));
        self.frame_count += 1;

        match self.run() {
            Ok(result) => Ok(result),
            Err(traceback) => Err(traceback.to_string()),
        }
    }

    fn next_number(&self) -> usize {
        self.frames[self.frame_count - 1]
            .closure
            .as_ref()
            .borrow()
            .function
            .chunk
            .opcodes[self.frames[self.frame_count - 1].ip] as usize
    }

    pub fn run(&mut self) -> Result<Value, Trace> {
        let mut result = Value::Unit;

        loop {
            //self.stack.debug_stack();

            match self.decode_opcode() {
                Opcode::Const => {
                    let index = self.next_number();
                    self.next();
                    self.stack.push(
                        self.frames[self.frame_count - 1]
                            .closure
                            .as_ref()
                            .borrow()
                            .function
                            .chunk
                            .constants[index]
                            .clone(),
                    );
                }
                Opcode::True => self.stack.push(Value::Boolean(true)),
                Opcode::False => self.stack.push(Value::Boolean(false)),
                Opcode::Nil => self.stack.push(Value::Nil),
                Opcode::Unit => self.stack.push(Value::Unit),
                Opcode::Add => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs + rhs)
                }
                Opcode::Sub => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs - rhs)
                }
                Opcode::Mul => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs * rhs)
                }
                Opcode::Div => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs / rhs)
                }
                Opcode::Mod => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs % rhs)
                }
                Opcode::Negate => {
                    let val = self.stack.pop();
                    self.stack.push(-val)
                }
                Opcode::Equal => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Value::Boolean(lhs == rhs))
                }
                Opcode::NotEqual => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Value::Boolean(lhs != rhs))
                }
                Opcode::Gte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Value::Boolean(lhs >= rhs))
                }
                Opcode::Lte => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Value::Boolean(lhs <= rhs))
                }
                Opcode::Gt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Value::Boolean(lhs > rhs))
                }
                Opcode::Lt => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(Value::Boolean(lhs < rhs))
                }
                Opcode::Not => {
                    let val = self.stack.pop();
                    self.stack.push(!val)
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
                    let result = match context.globals.get(&name.to_string()) {
                        Some(val) => self.stack.push(val.clone()),
                        None => match context.prelude.get(&name.to_string()) {
                            Ok(val) => self.stack.push(val.clone()),
                            Err(_) => panic!("cannot find '{}' in this scope", &name.to_string()),
                        },
                    };
                    self.frames[self.frame_count - 1].ip += 1;
                    result
                }
                Opcode::SaveLocal => {
                    let data = self.stack.pop();

                    let index = self.next_number();
                    self.stack
                        .save_local(index + self.frames[self.frame_count - 1].base_ip, data);

                    self.next();
                }
                Opcode::LoadLocal => {
                    let index = self.next_number();
                    let slot = self
                        .stack
                        .get(index + self.frames[self.frame_count - 1].base_ip);

                    self.next();
                    self.stack.push(slot);
                }
                Opcode::SaveUpValue => {
                    let index = self.next_number();
                    let value = self.stack.pop();
                    self.frames[self.frame_count - 1]
                        .closure
                        .as_ref()
                        .borrow_mut()
                        .captures[index]
                        .value = Rc::new(value);

                    self.next();
                }
                Opcode::LoadUpValue => {
                    //println!("{:#?}", self.frames);

                    let index = self.next_number();
                    let data = self.frames[self.frame_count - 1]
                        .closure
                        .as_ref()
                        .borrow()
                        .captures[index]
                        .borrow()
                        .to_owned();

                    self.next();
                    self.stack.push(data.value.as_ref().borrow().clone());
                }
                Opcode::CloseUpValue => {
                    //let last = self.stack.pop();
                    self.close_upvalues(self.stack.len() - 1);
                    self.stack.pop();
                }
                Opcode::Loop => {
                    self.frames[self.frame_count - 1].ip -= self.read_short();
                }
                Opcode::Jump => {
                    self.frames[self.frame_count - 1].ip += self.read_short();
                }
                Opcode::JumpIfFalse => {
                    let base_ip = self.read_short();
                    if self.is_falsy() {
                        self.frames[self.frame_count - 1].ip += base_ip;
                    }
                }
                Opcode::JumpIfTrue => {
                    let base_ip = self.read_short();
                    if !self.is_falsy() {
                        self.frames[self.frame_count - 1].ip += base_ip;
                    }
                }
                Opcode::Import => self.import()?,
                Opcode::Class => self.class()?,
                Opcode::Constructor => {
                    let closure = match self.stack.pop() {
                        Value::Closure(closure) => closure,
                        _ => panic!("expected closure"),
                    };

                    let name = closure.as_ref().borrow().function.name.clone();

                    self.stack.push(Value::Constructor(Rc::new(Constructor::new(
                        name.to_string(),
                        closure,
                    ))));
                }
                Opcode::Field => {
                    todo!()
                }
                Opcode::Instance => {
                    todo!()
                }
                Opcode::Method => {
                    todo!()
                }
                Opcode::Call => self.call()?,
                Opcode::Closure => self.closure()?,
                Opcode::Return => self.return_(),
                Opcode::List => self.list()?,
                Opcode::BuildTuple => self.tuple()?,
                Opcode::BuildMap => self.map()?,
                Opcode::GetIndex => self.get_index()?,
                Opcode::SetIndex => self.set_index()?,
                Opcode::Get => self.get()?,
                Opcode::Set => self.set()?,
                Opcode::Del => {
                    result = self.stack.pop();
                }
                Opcode::Halt => break,
            };
        }

        Ok(result)
    }

    fn closure(&mut self) -> Result<(), Trace> {
        let fun = match self.get_constant() {
            Value::Function(fun) => fun,
            _ => panic!("expected a function"),
        };

        let mut closure = Closure::wrap(fun);

        for captured in closure.function.captures.iter() {
            let reference = match captured {
                Captured::Local(index) => self.capture_upvalue(*index),
                Captured::NonLocal(index) => self.frames[self.frame_count - 1]
                    .closure
                    .as_ref()
                    .borrow()
                    .captures[*index]
                    .clone(),
            };

            closure.captures.push(reference);
        }

        self.next();
        self.stack
            .push(Value::Closure(Rc::new(RefCell::new(closure))));
        Ok(())
    }

    fn call(&mut self) -> Result<(), Trace> {
        let arity = self.next_number();
        self.frames[self.frame_count - 1].ip += 1;

        match self.stack.pop() {
            Value::NativeFun(fun) => Ok(self.ffi_call(fun, arity)),
            Value::Closure(closure) => {
                self.fun_call(closure);
                Ok(())
            }
            Value::Constructor(constructor) => Ok(self.constructor_call(constructor)),
            _ => Err(Trace::new("can only call functions", self.frames.clone())),
        }
    }

    fn ffi_call(&mut self, fun: Rc<NativeFun>, arity: usize) {
        let mut args = vec![];
        for _ in 0..arity {
            args.push(self.stack.pop());
        }

        let result = fun.fun.0(Rc::clone(&self.context), args);
        self.stack.push(result);
    }

    fn constructor_call(&mut self, constructor: Rc<Constructor>) {
        self.fun_call(constructor.closure.clone());

        let class = constructor.class.clone().unwrap();

        let mut instance = Instance::new(class.clone());

        for field in &class.fields {
            instance.add_field(field.0.to_owned(), field.1.to_owned());
        }

        self.stack.push(Value::Instance(Rc::new(instance)));
    }

    fn fun_call(&mut self, closure: Rc<RefCell<Closure>>) {
        let arity = closure.as_ref().borrow().function.arity;

        let frame = Frame::new(closure, 0, self.stack.len() - arity);

        self.frame_count += 1;
        self.frames.push(frame);
    }

    fn return_(&mut self) {
        let return_val = self.stack.pop();

        for i in self.frames[self.frame_count - 1].base_ip..self.stack.stack.len() {
            self.close_upvalues(i);
        }

        self.next();

        self.stack
            .stack
            .truncate(self.frames[self.frame_count - 1].base_ip);

        self.frames.pop().unwrap();

        self.frame_count -= 1;

        self.stack.push(return_val);
    }

    fn capture_upvalue(&mut self, index: usize) -> Upvalue {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalues.clone();

        while upvalue.is_some() && upvalue.as_ref().unwrap().position > index {
            prev_upvalue = upvalue.clone();
            upvalue = upvalue.unwrap().next();
        }

        if let Some(upvalue) = upvalue.clone() {
            if upvalue.position == index {
                return upvalue;
            }
        }

        let new_upvalue = Upvalue::new(
            Rc::new(
                self.stack
                    .get(self.frames[self.frame_count - 1].base_ip + index),
            ),
            upvalue.map(Rc::new),
            index,
        );

        if let Some(mut prev_upvalue) = prev_upvalue {
            prev_upvalue.next = Some(Rc::new(new_upvalue.clone()));
        } else {
            self.open_upvalues = Some(new_upvalue.clone());
        }

        new_upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        while self.open_upvalues.is_some() && self.open_upvalues.as_ref().unwrap().position >= last
        {
            let mut upvalue = self.open_upvalues.as_ref().unwrap().clone();

            upvalue.closed = upvalue.value.as_ref().borrow().clone();
            upvalue.value = Rc::new(upvalue.closed);

            self.open_upvalues = upvalue.next.as_ref().map(|v| v.as_ref().borrow().clone());
        }
    }

    fn class(&mut self) -> Result<(), Trace> {
        let class = match self.get_constant() {
            Value::Class(class) => class,
            _ => panic!("expected class"),
        };

        self.next();

        let constructors = self.next_number();
        self.next();

        let fields = self.next_number();
        self.next();

        for _ in 0..fields {
            let _name = self.stack.pop();
            let _value = self.stack.pop();

            //class.add_field(name.to_string(), value);
        }

        let _parent = class.clone();

        for _ in 0..constructors {
            let mut _constructor = match self.stack.pop() {
                Value::Constructor(constructor) => constructor,
                _ => panic!("expected constructor"),
            };

            //constructor.class = Some(parent.clone());

            //class.add_constructor(constructor.name.to_owned(), constructor);
        }

        self.stack.push(Value::Class(class));
        Ok(())
    }

    fn import(&mut self) -> Result<(), Trace> {
        self.stack.debug_stack();

        let name = self.get_constant();

        // first check the module cache
        /*if let Some(module) = self.loader.get_module(&name.to_string()) {
            println!("{:?}", module);

            return Ok(());
        }*/

        if let Ok(_value) = self.prelude().get(&name.to_string()) {
            return Ok(());
        }

        let _module = self.loader.compile_module(&name.to_string());

        Ok(())
    }

    fn list(&mut self) -> Result<(), Trace> {
        let mut list: Vec<Value> = vec![];
        let length = self.get_opcode(self.frames[self.frame_count - 1].ip) as usize;
        for _ in 0..length {
            list.push(self.stack.pop());
        }
        self.next();
        self.stack.push(Value::List(ValueList::from_vec(&list)));
        Ok(())
    }

    fn tuple(&mut self) -> Result<(), Trace> {
        let mut tuple = vec![];
        let length = self.get_opcode(self.frames[self.frame_count - 1].ip) as usize;
        for _ in 0..length {
            tuple.push(self.stack.pop());
        }

        self.next();
        self.stack.push(Value::Tuple(ValueTuple::new()));
        Ok(())
    }

    fn map(&mut self) -> Result<(), Trace> {
        let mut map = ValueMap::new();
        let length = self.get_opcode(self.frames[self.frame_count - 1].ip) as usize;
        for _ in 0..length {
            let key = self.stack.pop();
            let value = self.stack.pop();
            map.insert_constant(&key.to_string(), value);
        }

        self.next();
        self.stack.push(Value::Map(Rc::new(map)));
        Ok(())
    }

    fn get_index(&mut self) -> Result<(), Trace> {
        let index = self.stack.pop();
        let expr = self.stack.pop();

        match index {
            Value::Number(index) => {
                match expr {
                    Value::List(list) => {
                        let length = list.len();
                        self.bounds_check(length, index)?;

                        // if index is negative, index backwards into list
                        if index.is_sign_negative() {
                            Ok(self.stack.push(
                                list.0.as_ref().borrow()[length - index.abs() as usize].clone(),
                            ))
                        } else {
                            Ok(self
                                .stack
                                .push(list.0.as_ref().borrow()[index as usize].clone()))
                        }
                    }
                    val => Err(Trace::new(
                        &format!("cannot index into {val}"),
                        self.frames.clone(),
                    )),
                }
            }
            _ => Err(Trace::new("can only index into lists", self.frames.clone())),
        }
    }

    fn set_index(&mut self) -> Result<(), Trace> {
        let index = self.stack.pop();
        let expr = self.stack.pop();
        let value = self.stack.pop();

        match expr {
            Value::List(list) => {
                match index {
                    Value::Number(index) => {
                        let length = list.len();
                        self.bounds_check(length, index)?;

                        // if index is negative, index backwards into list
                        if index.is_sign_negative() {
                            list.0.borrow_mut()[length - index.abs() as usize] = value;
                            self.stack.push(Value::List(list))
                        } else {
                            list.0.borrow_mut()[index as usize] = value;
                            self.stack.push(Value::List(list))
                        }
                    }
                    _ => {
                        return Err(Trace::new(
                            "lists can only be indexed by a number",
                            self.frames.clone(),
                        ))
                    }
                };
            }
            _ => return Err(Trace::new("can only index into lists", self.frames.clone())),
        }

        Ok(())
    }

    fn bounds_check<T: Into<f64>>(&self, length: usize, index: T) -> Result<(), Trace> {
        let index = index.into();

        // upper bounds check
        if index.is_sign_positive() && length as f64 <= index {
            return Err(Trace::new(
                &format!("index out of bounds: the length is {length} but the index is {index}"),
                self.frames.clone(),
            ));
        }

        // lower bounds check
        if index.is_sign_negative() && length as f64 <= index.abs() - 1. {
            return Err(Trace::new(
                &format!("index out of bounds: the length is {length} but the index is {index}"),
                self.frames.clone(),
            ));
        }

        Ok(())
    }

    fn get(&mut self) -> Result<(), Trace> {
        match self.stack.pop() {
            Value::Map(map) => self.stack.push(
                map.get(&self.get_constant().to_string()[..])
                    .unwrap()
                    .clone(),
            ),
            Value::String(val) => {
                self.stack.push(Value::String(val));
                if let Value::Map(map) = self
                    .context
                    .as_ref()
                    .borrow_mut()
                    .prelude
                    .get("string")
                    .unwrap()
                {
                    self.stack.push(
                        map.get(&self.get_constant().to_string()[..])
                            .unwrap()
                            .clone(),
                    );
                } else {
                    unreachable!()
                }
            }
            Value::List(list) => {
                self.stack.push(Value::List(list));
                if let Value::Map(map) = self
                    .context
                    .as_ref()
                    .borrow_mut()
                    .prelude
                    .get("list")
                    .unwrap()
                {
                    let value = map.get(&self.get_constant().to_string()[..]);
                    if let Ok(value) = value {
                        self.stack.push(value.clone());
                    } else {
                        return Err(Trace::new(&value.unwrap_err(), self.frames.clone()));
                    }
                } else {
                    unimplemented!()
                }
            }
            Value::Tuple(tuple) => {
                self.stack.push(Value::Tuple(tuple));
                if let Value::Map(map) = self
                    .context
                    .as_ref()
                    .borrow_mut()
                    .prelude
                    .get("tuple")
                    .unwrap()
                {
                    self.stack.push(
                        map.get(&self.get_constant().to_string()[..])
                            .unwrap()
                            .clone(),
                    );
                } else {
                    unimplemented!()
                }
            }
            Value::Class(class) => {
                let constructor = match class.constructors.get(&self.get_constant().to_string()) {
                    Some(constructor) => constructor.to_owned(),
                    None => panic!("expected constructor"),
                };

                self.stack.push(Value::Constructor(Rc::new(constructor)));
            }
            Value::Instance(instance) => {
                let value = instance.get_field(&self.get_constant().to_string());
                self.stack.push(value);
            }
            Value::External(external) => {
                self.stack.push(Value::External(external.clone()));
                self.stack.push(
                    external
                        .meta_map
                        .as_ref()
                        .borrow_mut()
                        .get(&self.get_constant().to_string()[..]),
                );
            }
            _ => return Err(Trace::new("can only index into a map", self.frames.clone())),
        };

        self.next();
        Ok(())
    }

    fn set(&mut self) -> Result<(), Trace> {
        Ok(())
    }

    fn read_short(&mut self) -> usize {
        self.frames[self.frame_count - 1].ip += 2;
        let base_ip = ((self.frames[self.frame_count - 1]
            .closure
            .as_ref()
            .borrow()
            .function
            .chunk
            .opcodes[self.frames[self.frame_count - 1].ip - 2] as u16)
            << 8)
            | self.frames[self.frame_count - 1]
                .closure
                .as_ref()
                .borrow()
                .function
                .chunk
                .opcodes[self.frames[self.frame_count - 1].ip - 1] as u16;
        base_ip as usize
    }

    #[inline]
    fn is_falsy(&mut self) -> bool {
        matches!(self.stack.peek(), Value::Boolean(false))
    }

    #[inline]
    fn next(&mut self) {
        self.frames[self.frame_count - 1].ip += 1;
    }

    #[inline]
    fn get_opcode(&mut self, index: usize) -> u8 {
        self.frames[self.frame_count - 1]
            .closure
            .as_ref()
            .borrow()
            .function
            .chunk
            .opcodes[index]
    }

    #[inline]
    fn get_constant(&self) -> Value {
        self.frames[self.frame_count - 1]
            .closure
            .as_ref()
            .borrow()
            .function
            .chunk
            .constants[self.next_number()]
        .clone()
    }

    #[inline]
    fn decode_opcode(&mut self) -> Opcode {
        self.next();
        Opcode::from(
            self.frames[self.frame_count - 1]
                .closure
                .as_ref()
                .borrow()
                .function
                .chunk
                .opcodes[self.frames[self.frame_count - 1].ip - 1],
        )
    }

    pub fn prelude(&self) -> ValueMap {
        self.context.as_ref().borrow_mut().prelude.clone()
    }
}
