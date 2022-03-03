use std::{rc::Rc, cell::RefCell};

use crate::common::{Value, Closure};

#[derive(Clone, Debug)]
pub struct Frame {
    pub closure: Rc<RefCell<Closure>>,
    pub ip: usize,
    pub base_ip: usize,
}

impl Frame {
    pub fn new(fun: Rc<RefCell<Closure>>, ip: usize, base_ip: usize) -> Self {
        Frame {
            closure: fun.clone(),
            ip,
            base_ip,
        }
    }
}

#[derive(Debug, Default)]
pub struct Stack {
    pub stack: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Stack { stack: vec![] }
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack should not be empty")
    }

    #[inline]
    pub fn push(&mut self, value: Value) -> Value {
        self.stack.push(value);
        self.stack[self.stack.len() - 1].clone()
    }

    #[inline]
    pub fn peek(&mut self) -> Value {
        self.stack[self.stack.len() - 1].clone()
    }

    #[inline]
    pub fn set(&mut self, idx: usize, value: Value) {
        self.stack[idx] = value;
    }

    #[inline]
    pub fn get(&mut self, idx: usize) -> Value {
        self.stack[idx].clone()
    }

    #[inline]
    pub fn save_local(&mut self, idx: usize, value: Value) {
        self.stack[idx] = value;
    }

    /// Prints the current stack to stdout.
    pub fn debug_stack(&self) {
        if self.stack.is_empty() {
            println!("[]");
        } else {
            println!(
                "{}",
                &self
                    .stack
                    .iter()
                    .map(|s| format!("[ {} ]", s))
                    .collect::<Vec<String>>()
                    .join("")
            );
        }
    }
}
