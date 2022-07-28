use std::{borrow::Borrow, rc::Rc};

use crate::{
    common::{
        value::{CallableFunction, ValueList, ValueTuple},
        BoundMethod, Captured, Class, Closure, Constructor, ImmutableString, Instance, Map,
        NativeFun, Opcode, ToValue, Upvalue,
    },
    Value,
};

use super::{Frame, Trace, Vm};

impl Vm {
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

    #[inline]
    fn next(&mut self) {
        self.frames[self.frame_count - 1].ip += 1;
    }

    fn next_number(&self) -> usize {
        self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .opcodes[self.frames[self.frame_count - 1].ip] as usize
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
    fn is_falsy(&self) -> bool {
        matches!(self.stack.peek(), Value::Boolean(false))
    }

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

    fn def_global(&mut self) {
        let name = &*self.frames[self.frame_count - 1]
            .closure
            .function
            .chunk
            .variables[self.next_number()];

        self.last_module.add_const(name, self.stack.pop());

        self.next();
    }

    fn set_global(&mut self) {
        let name = self.get_constant();

        let index = self.last_module.get_index(name);

        if let Some(index) = index {
            self.last_module.set_const(index, self.stack.pop());
        } else {
            panic!("Cannot assign to undefined variable");
        }

        self.next();
    }

    fn get_global(&mut self) -> Result<(), Trace> {
        let name = self.get_constant();
        let index = self.last_module.get_index(name);

        if let Some(index) = index {
            self.stack.push(self.last_module.get_const(index).clone());
        } else {
            return Err(Trace::new(
                &format!("Cannot find {name}"),
                self.frames.clone(),
            ));
        }

        self.next();

        Ok(())
    }

    fn save_local(&mut self) {
        let data = self.stack.pop();

        let index = self.next_number();
        self.stack
            .save_local(index + self.frames[self.frame_count - 1].base_ip, data);

        self.next();
    }

    fn load_local(&mut self) {
        let index = self.next_number();
        let slot = self
            .stack
            .get(index + self.frames[self.frame_count - 1].base_ip);

        self.next();
        self.stack.push(slot);
    }

    fn save_upvalue(&mut self) {
        let index = self.next_number();
        let value = self.stack.pop();
        self.frames[self.frame_count - 1]
            .closure
            .captures
            .borrow_mut()[index]
            .value = Rc::new(value);

        self.next();
    }

    fn load_upvalue(&mut self) {
        let index = self.next_number();
        let data = self.frames[self.frame_count - 1].closure.captures.borrow()[index]
            .borrow()
            .to_owned();

        self.next();
        self.stack.push(data.value.as_ref().borrow().clone());
    }

    fn call_value(&mut self, arity: usize) -> Result<(), Trace> {
        match self.stack.get(self.stack.len() - 1 - arity) {
            Value::NativeFun(fun) => self.call_native(fun, arity),
            Value::Closure(closure) => self.call_closure(closure, arity),
            Value::Constructor(constructor) => self.call_ctor(constructor),
            Value::Method(method) => self.call_method(method, arity),
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

    fn call_native(&mut self, fun: Rc<NativeFun>, arity: usize) {
        let mut args = vec![];
        for _ in 0..arity {
            args.push(self.stack.pop());
        }

        let result = fun.call(self, args);
        self.stack.pop();
        self.stack.push(result);
    }

    fn call_ctor(&mut self, init: Rc<Constructor>) {
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
    fn call_method(&mut self, bound: Rc<BoundMethod>, arity: usize) {
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
                    .collect::<Vec<_>>();

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
    fn call_closure(&mut self, closure: Rc<Closure>, arity: usize) {
        let frame = Frame::new(closure, 0, self.stack.len() - arity);

        self.frame_count += 1;
        self.frames.push(frame);
    }

    fn make_closure(&mut self) -> Result<(), Trace> {
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

    fn make_class(&mut self) -> Result<(), Trace> {
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

    fn make_list(&mut self) -> Result<(), Trace> {
        let mut list: Vec<Value> = vec![];
        let length = self.get_opcode(self.frames[self.frame_count - 1].ip) as usize;
        for _ in 0..length {
            list.push(self.stack.pop());
        }
        self.next();
        self.stack.push(Value::List(ValueList::from_vec(&list)));
        Ok(())
    }

    fn make_tuple(&mut self) -> Result<(), Trace> {
        let mut tuple = vec![];
        let length = self.get_opcode(self.frames[self.frame_count - 1].ip) as usize;
        for _ in 0..length {
            tuple.push(self.stack.pop());
        }

        self.next();
        self.stack.push(Value::Tuple(ValueTuple::new()));
        Ok(())
    }

    fn make_map(&mut self) -> Result<(), Trace> {
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

    fn make_string(&mut self) -> Result<(), Trace> {
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

    fn capture_upvalue(&mut self, index: usize) -> Upvalue {
        let new_upvalue = Upvalue::new(
            Rc::new(
                self.stack
                    .get(self.frames[self.frame_count - 1].base_ip + index),
            ),
            None,
            index,
        );

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

    fn import(&mut self) -> Result<(), Trace> {
        todo!()
    }

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
                let class = self.builtins.map.clone();

                let method = Instance::builtin(value, name, class);
                self.stack.push(Value::Method(Rc::new(method)));
            }
            value @ Value::String(_) => {
                let name = self.get_constant();
                let class = self.builtins.string.clone();

                let method = Instance::builtin(value, name, class);
                self.stack.push(Value::Method(Rc::new(method)));
            }
            value @ Value::Float(_) => {
                let name = self.get_constant();
                let class = self.builtins.float.clone();

                let method = Instance::builtin(value, name, class);
                self.stack.push(Value::Method(Rc::new(method)));
            }
            Value::List(_) => {
                todo!()
            }
            Value::Tuple(tuple) => {
                self.stack.push(Value::Tuple(tuple));

                todo!()
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

        if let Some(instance) = self.stack.pop().as_instance() {
            instance
                .fields
                .borrow_mut()
                .insert(name.into(), self.stack.pop());
        }

        self.next();

        Ok(())
    }

    pub fn run(&mut self) -> Result<Value, Trace> {
        let mut result = Value::Unit;

        loop {
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
                    self.def_global();
                }
                Opcode::SetGlobal => {
                    self.set_global();
                }
                Opcode::GetGlobal => {
                    self.get_global()?;
                }
                Opcode::SaveLocal => {
                    self.save_local();
                }
                Opcode::LoadLocal => {
                    self.load_local();
                }
                Opcode::SaveUpValue => {
                    self.save_upvalue();
                }
                Opcode::LoadUpValue => {
                    self.load_upvalue();
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
                Opcode::Call => {
                    let arity = self.next_number();
                    self.next();

                    self.call_value(arity)?;
                }
                Opcode::Call0 => self.call_value(0)?,
                Opcode::Call1 => self.call_value(1)?,
                Opcode::Call2 => self.call_value(2)?,
                Opcode::Closure => self.make_closure()?,
                Opcode::Class => self.make_class()?,
                Opcode::List => self.make_list()?,
                Opcode::Tuple => self.make_tuple()?,
                Opcode::String => self.make_string()?,
                Opcode::Map => self.make_map()?,
                Opcode::Return => self.return_(),
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

        Ok(result)
    }
}
