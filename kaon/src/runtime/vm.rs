use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::rc::Rc;
use std::u8;

use fnv::FnvHashMap;

use crate::common::state::State;
use crate::common::value::{CallableFunction, ToValue, ValueList, ValueTuple};
use crate::common::{
    BoundMethod, Captured, Class, Closure, Constructor, Function, ImmutableString, Instance,
    KaonFile, Map, NativeFun, Opcode, Upvalue, Value, Named,
};
use crate::core::{self};
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
    pub prelude: State,
}

impl VmContext {
    pub fn with_settings(settings: VmSettings) -> Self {
        let prelude = core::prelude();

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
            open_upvalues: None,
        }
    }

    pub fn with_settings(settings: VmSettings) -> Vm {
        Vm {
            context: Rc::new(RefCell::new(VmContext::with_settings(settings))),
            ..Vm::default()
        }
    }

    /// Clear the VM's state.
    pub fn clear(&mut self) {
        self.frames.clear();
        self.stack.clear();
        self.frame_count = 0;
        self.open_upvalues = None;
    }

    /// Run a chunk of bytecode.
    pub fn execute(&mut self, fun: Rc<Function>) -> Result<Value, String> {
        self.frames
            .push(Frame::new(Rc::new(Closure::wrap(fun.clone())), 0, 1));
        self.frame_count += 1;

        self.stack.push(Value::Function(fun));

        match self.run() {
            Ok(result) => Ok(result),
            Err(traceback) => Err(traceback.to_string()),
        }
    }

    /// Build a number from the bytecode stream.
    fn next_number(&self) -> usize {
        self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .opcodes[self.frames[self.frame_count - 1].ip] as usize
    }

    /// The main VM loop.
    pub fn run(&mut self) -> Result<Value, Trace> {
        let mut result = Value::Unit;

        loop {
            #[cfg(debug_assertions)]
            {
                self.debug_stack();
            }

            match self.decode_opcode() {
                Opcode::Const => {
                    let index = self.next_number();
                    self.next();
                    self.stack.push(
                        *self.frames[self.frame_count - 1]
                            .closure
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
                Opcode::String => self.load_string()?,
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
                Opcode::BitAnd => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs & rhs);
                }
                Opcode::BitOr => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs | rhs);
                }
                Opcode::BitXor => {
                    let lhs = self.stack.pop();
                    let rhs = self.stack.pop();
                    self.stack.push(lhs ^ rhs);
                }
                Opcode::DefGlobal => {
                    let name = self.get_constant();
                    self.context
                        .as_ref()
                        .borrow_mut()
                        .globals
                        .insert(name.to_string(), self.stack.pop());

                    self.next();
                }
                Opcode::SetGlobal => {
                    let name = self.get_constant();
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
                    let result = match context.globals.get(name) {
                        Some(val) => self.stack.push(val.clone()),
                        None => match context.prelude.get::<Value>(name) {
                            Some(val) => self.stack.push(val.clone()),
                            None => {
                                return Err(Trace::new(
                                    &format!("Cannot find {name}"),
                                    self.frames.clone(),
                                ))
                            }
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
                        .captures
                        .borrow_mut()[index]
                        .value = Rc::new(value);

                    self.next();
                }
                Opcode::LoadUpValue => {
                    let index = self.next_number();
                    let data = self.frames[self.frame_count - 1].closure.captures.borrow()[index]
                        .borrow()
                        .to_owned();

                    self.next();
                    self.stack.push(data.value.as_ref().borrow().clone());
                }
                Opcode::CloseUpValue => {
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
                Opcode::Call => {
                    let arity = self.next_number();
                    self.next();

                    self.call(arity)?;
                }
                Opcode::Call0 => self.call(0)?,
                Opcode::Call1 => self.call(1)?,
                Opcode::Call2 => self.call(2)?,
                Opcode::Closure => self.closure()?,
                Opcode::Return => self.return_(),
                Opcode::List => self.list()?,
                Opcode::Tuple => self.tuple()?,
                Opcode::Map => self.map()?,
                Opcode::GetIndex => self.get_index()?,
                Opcode::SetIndex => self.set_index()?,
                Opcode::Get => self.get()?,
                Opcode::Set => self.set()?,
                Opcode::Pop => {
                    result = self.stack.pop();
                }
                Opcode::PopN => {
                    let num = self.next_number();
                    self.stack.truncate(num);

                    self.next();
                }
                Opcode::Halt => break,
            };
        }

        //#[cfg(debug_assertions)]
        //#[cfg(not(test))]
        //assert_eq!(self.stack.len(), 1);

        Ok(result)
    }

    /// Construct a closure from a function.
    fn closure(&mut self) -> Result<(), Trace> {
        let fun = match &*self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .constants[self.next_number()]
        {
            Value::Function(fun) => fun,
            _ => panic!("expected a function"),
        };

        let closure = Closure::wrap(fun.clone());

        for captured in closure.function.captures.iter() {
            let reference = match captured {
                Captured::Local(index) => self.capture_upvalue(*index),
                Captured::NonLocal(index) => {
                    self.frames[self.frame_count - 1].closure.captures.borrow()[*index].clone()
                }
            };

            closure.captures.borrow_mut().push(reference);
        }

        self.next();
        self.stack.push(Value::Closure(Rc::new(closure)));
        Ok(())
    }

    /// Call the value off the top of the stack.
    fn call(&mut self, arity: usize) -> Result<(), Trace> {
        match self.stack.get(self.stack.len() - 1 - arity) {
            Value::NativeFun(fun) => self.native_call(fun, arity),
            Value::Closure(closure) => self.fun_call(closure, arity),
            Value::Constructor(constructor) => self.constructor_call(constructor),
            Value::Method(method) => self.method_call(method, arity),
            Value::Class(class) => {
                self.stack.pop();
                let instance = class.instance();

                self.stack.push(Value::Instance(instance));
            }
            v => {
                println!("{v}");
                return Err(Trace::new("can only call functions", self.frames.clone()));
            }
        }

        Ok(())
    }

    /// Call a foreign function.
    fn native_call(&mut self, fun: Rc<NativeFun>, arity: usize) {
        let mut args = vec![];
        for _ in 0..arity {
            args.push(self.stack.pop());
        }

        let result = fun.call(self, args);
        self.stack.pop();
        self.stack.push(result);
    }

    /// Call a constructor.
    fn constructor_call(&mut self, init: Rc<Constructor>) {
        let function = match &init.function {
            CallableFunction::Native(fun) => {
                let args = self
                    .stack
                    .stack
                    .drain(fun.arity()..)
                    .rev()
                    .collect::<Vec<Value>>();
                let result = fun.call(self, args);

                self.stack.pop();

                self.stack.push(result);

                return;
            }
            CallableFunction::Function(f) => f,
        };

        let instance = init.class.clone().instance();

        let frame = Frame::new(
            function.clone(),
            0,
            self.stack.len() - function.function.arity,
        );

        self.frame_count += 1;
        self.frames.push(frame);

        // Array insertion is O(n).
        // Is there a more efficent solution?
        self.stack.stack.insert(
            self.stack.len() - function.function.arity,
            instance.to_value(),
        );
    }

    /// Call a method
    fn method_call(&mut self, bound: Rc<BoundMethod>, arity: usize) {
        let function = match &bound.function {
            CallableFunction::Native(fun) => {
                let arity = if fun.is_varidic {
                    arity + 1
                } else {
                    fun.arity()
                };

                let mut args = self
                    .stack
                    .stack
                    .drain(self.stack.len() - arity + 1..)
                    //.rev()
                    .collect::<Vec<Value>>();

                let mut arg_list = vec![bound.receiver.clone()];
                arg_list.append(&mut args);

                let result = fun.call(self, arg_list);

                self.stack.pop();
                self.stack.push(result);

                return;
            }
            CallableFunction::Function(f) => f,
        };

        let frame = Frame::new(
            function.clone(),
            0,
            self.stack.len() - function.function.arity,
        );

        self.frame_count += 1;
        self.frames.push(frame);

        self.stack.stack.insert(
            self.stack.len() - function.function.arity,
            bound.receiver.clone(),
        );
    }

    /// Call a function.
    fn fun_call(&mut self, closure: Rc<Closure>, arity: usize) {
        let frame = Frame::new(closure, 0, self.stack.len() - arity);

        self.frame_count += 1;
        self.frames.push(frame);
    }

    /// Return from a function.
    fn return_(&mut self) {
        let return_val = self.stack.pop();

        for i in self.frames[self.frame_count - 1].base_ip..self.stack.stack.len() {
            self.close_upvalues(i);
        }

        self.next();
        let frame = self.frames.pop().unwrap();

        self.stack.truncate(frame.base_ip);

        self.frame_count -= 1;

        self.stack.pop();
        self.stack.push(return_val);
    }

    /// Capture an upvalue from the stack.
    fn capture_upvalue(&mut self, index: usize) -> Upvalue {
        /*let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalues.clone();

        while upvalue.is_some() && upvalue.as_ref().unwrap().position > index {
            prev_upvalue = upvalue.clone();
            upvalue = upvalue.unwrap().next();
        }

        if let Some(upvalue) = upvalue.clone() {
            if upvalue.position == index {
                return upvalue;
            }
        }*/

        let new_upvalue = Upvalue::new(
            Rc::new(
                self.stack
                    .get(self.frames[self.frame_count - 1].base_ip + index),
            ),
            None, //upvalue.map(Rc::new),
            index,
        );

        /*if let Some(mut prev_upvalue) = prev_upvalue {
            prev_upvalue.next = Some(Rc::new(new_upvalue.clone()));
        } else {
            self.open_upvalues = Some(new_upvalue.clone());
        }*/

        new_upvalue
    }

    /// Close over an upvalue.
    fn close_upvalues(&mut self, last: usize) {
        while self.open_upvalues.is_some() && self.open_upvalues.as_ref().unwrap().position >= last
        {
            let mut upvalue = self.open_upvalues.as_ref().unwrap().clone();

            upvalue.closed = upvalue.value.as_ref().borrow().clone();
            upvalue.value = Rc::new(upvalue.closed);

            self.open_upvalues = upvalue.next.as_ref().map(|v| v.as_ref().borrow().clone());
        }
    }

    /// Create a new class.
    fn class(&mut self) -> Result<(), Trace> {
        let index = self.next_number();
        let name = &*self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .variables[index];

        let class = Class::new(name);
        self.next();

        let num_methods = self.next_number();
        self.next();

        let num_constructors = self.next_number();
        self.next();

        let num_fields = self.next_number();
        self.next();

        for i in (0..num_fields).rev() {
            let name =
                &*self.frames.last().unwrap().closure.function.chunk.variables[index + i + 1];
            let value = self.stack.pop();

            class.add_field(name, value);
        }

        for _ in 0..num_methods {
            let method = self.stack.pop();

            class.add_method(
                method.as_closure().unwrap().function.name.as_str(),
                CallableFunction::Function(method.as_closure().unwrap()),
            );
        }

        for _ in 0..num_constructors {
            let constructor = self.stack.pop();

            class.add_init(
                constructor.as_closure().unwrap().function.name.as_str(),
                CallableFunction::Function(constructor.as_closure().unwrap()),
            );
        }

        self.stack.push(Value::Class(class));

        Ok(())
    }

    /// Handle imports.
    fn import(&mut self) -> Result<(), Trace> {
        todo!()
    }

    /// Create a list.
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

    /// Create a tuple.
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

    /// Build the map.
    fn map(&mut self) -> Result<(), Trace> {
        let length = self.get_opcode(self.frames[self.frame_count - 1].ip) as usize;
        let mut map = Map::with_capacity(length);
        for _ in 0..length {
            let key = self.stack.pop();
            let value = self.stack.pop();
            map.insert(key.to_string(), value);
        }

        self.next();
        self.stack.push(Value::Map(map));
        Ok(())
    }

    /// Index into a list on the stack.
    fn get_index(&mut self) -> Result<(), Trace> {
        let index = self.stack.pop();
        let expr = self.stack.pop();

        match index {
            Value::Float(index) => {
                match expr {
                    Value::List(list) => {
                        let length = list.len();
                        self.bounds_check(length, index)?;

                        // if index is negative, index backwards into list
                        if index.is_sign_negative() {
                            self.stack.push(
                                list.0.as_ref().borrow()[length - index.abs() as usize].clone(),
                            );
                            Ok(())
                        } else {
                            self.stack
                                .push(list.0.as_ref().borrow()[index as usize].clone());
                            Ok(())
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

    /// Update the list at the giving index.
    fn set_index(&mut self) -> Result<(), Trace> {
        let index = self.stack.pop();
        let expr = self.stack.pop();
        let value = self.stack.pop();

        match expr {
            Value::List(list) => {
                match index {
                    Value::Float(index) => {
                        let length = list.0.as_ref().borrow().len();
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

    /// Bounds check a list.
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

    /// Handle the get opcode.
    fn get(&mut self) -> Result<(), Trace> {
        match self.stack.pop() {
            value @ Value::Map(_) => {
                let name = self.get_constant();

                let class = RefCell::borrow(self.context.as_ref())
                    .prelude
                    .get(Map::NAME)
                    .unwrap();

                let method = Instance::builtin(value, name, class);
                self.stack.push(Value::Method(Rc::new(method)));
            }
            value @ Value::String(_) => {
                let name = self.get_constant();

                let class = RefCell::borrow(self.context.as_ref())
                    .prelude
                    .get(ImmutableString::NAME)
                    .unwrap();

                let method = Instance::builtin(value, name, class);
                self.stack.push(Value::Method(Rc::new(method)));
            }
            value @ Value::Float(_) => {
                let name = self.get_constant();

                let class = RefCell::borrow(self.context.as_ref())
                    .prelude
                    .get("Float")
                    .unwrap();

                let method = Instance::builtin(value, name, class);
                self.stack.push(Value::Method(Rc::new(method)));
            }
            Value::List(_) => {
                todo!()
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
                let name = self.get_constant();

                if let Some(init) = class.get_init(name) {
                    self.stack
                        .push(Value::Constructor(Rc::new(Constructor::new(class, init))));
                } else if let Some(static_fun) = class.get_static(name) {
                    match static_fun {
                        CallableFunction::Native(fun) => self.stack.push(Value::NativeFun(fun)),
                        CallableFunction::Function(fun) => self.stack.push(Value::Closure(fun)),
                    }
                } else {
                    return Err(Trace::new(
                        &format!("no method `{name}` found for class '{}' ", class.name)[..],
                        self.frames.clone(),
                    ));
                }
            }
            Value::Instance(instance) => {
                let name = &*self.frames[self.frame_count - 1]
                    .closure
                    .function
                    .chunk
                    .variables[self.next_number()];

                if instance.fields().get(name).is_some() {
                    self.stack
                        .push(instance.fields().get(name).unwrap().clone());

                    self.next();

                    return Ok(());
                }

                let method = Instance::bind(instance, name);

                self.stack.push(Value::Method(Rc::new(method)));
            }
            _ => return Err(Trace::new("can only index into a map", self.frames.clone())),
        };

        self.next();
        Ok(())
    }

    fn set(&mut self) -> Result<(), Trace> {
        let name = &*self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .variables[self.next_number()];

        match self.stack.pop() {
            Value::Instance(instance) => {
                instance
                    .fields
                    .borrow_mut()
                    .insert(name.into(), self.stack.pop());
            }
            _ => todo!(),
        }

        self.next();

        Ok(())
    }

    fn load_string(&mut self) -> Result<(), Trace> {
        let index = self.next_number();
        self.next();

        let s = &*self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .variables[index];

        self.stack.push(Value::String(ImmutableString::from(s)));

        Ok(())
    }

    /// Read a u16 from the stream of bytecode.
    fn read_short(&mut self) -> usize {
        self.frames[self.frame_count - 1].ip += 2;
        let base_ip = ((self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .opcodes[self.frames[self.frame_count - 1].ip - 2] as u16)
            << 8)
            | self.frames[self.frame_count - 1]
                .closure
                .function
                .chunk
                .opcodes[self.frames[self.frame_count - 1].ip - 1] as u16;
        base_ip as usize
    }

    #[inline]
    fn is_falsy(&self) -> bool {
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
            .function
            .chunk
            .opcodes[index]
    }

    #[inline]
    fn get_constant(&self) -> &str {
        &*self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .variables[self.next_number()]
    }

    #[inline]
    fn decode_opcode(&mut self) -> Opcode {
        self.next();
        Opcode::from(
            self.frames[self.frame_count - 1]
                .closure
                .function
                .chunk
                .opcodes[self.frames[self.frame_count - 1].ip - 1],
        )
    }

    pub fn debug_stack(&self) {
        let mut top = vec![];
        let mut stack = vec![];
        let mut bottom = vec![];

        for slot in &self.stack.stack {
            let length = slot.to_string().len() + 2;

            let header = format!("┌{}┐", "─".repeat(length));
            let slot = format!("│ {} │", slot);
            let border = format!("└{}┘", "─".repeat(length));

            top.push(header);
            stack.push(slot.to_owned());
            bottom.push(border);
        }

        println!("{}", top.join(""));
        println!("{}", stack.join(""));
        println!("{}", bottom.join(""));
    }

    pub fn exception_handler(&mut self, exception: &str) {
        panic!("{}", exception);
    }

    pub fn prelude(&self) -> &mut State {
        todo!()
    }
}
