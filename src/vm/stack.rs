use crate::common::{Value, Closure};

#[derive(Clone, Debug)]
pub struct Frame {
    pub closure: Closure,
    pub ip: usize,
    pub base_ip: usize,
}

impl Frame {
    pub fn new(fun: &mut Closure, ip: usize, base_ip: usize) -> Self {
        Frame {
            closure: fun.clone(),
            ip,
            base_ip,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Slot(pub Value);

impl Slot {
    pub fn new(data: Value) -> Slot {
        Slot(data)
    }

    pub fn get_data(&self) -> &Value {
        &self.0
    }
}

#[derive(Debug, Default)]
pub struct Stack {
    pub stack: Vec<Slot>,
}

impl Stack {
    pub fn new() -> Self {
        Stack { stack: vec![] }
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack should not be empty").0
    }

    #[inline]
    pub fn push(&mut self, slot: Slot) -> Value {
        self.stack.push(slot);
        self.stack[self.stack.len() - 1].0.clone()
    }

    #[inline]
    pub fn push_slot(&mut self, data: Value) -> Value {
        self.push(Slot::new(data))
    }

    #[inline]
    pub fn peek(&mut self) -> Value {
        self.stack[self.stack.len() - 1].0.clone()
    }

    #[inline]
    pub fn set(&mut self, idx: usize, data: Value) {
        self.stack[idx] = Slot::new(data);
    }

    #[inline]
    pub fn get(&mut self, idx: usize) -> Slot {
        self.stack[idx].clone()
    }

    #[inline]
    pub fn save_local(&mut self, idx: usize, data: Value) {
        self.stack[idx] = Slot::new(data);
    }

    // prints the current stack to stdout
    pub fn debug_stack(&self) {
        if self.stack.is_empty() {
            println!("[]");
        } else {
            println!(
                "{}",
                &self
                    .stack
                    .iter()
                    .map(|s| format!("[ {} ]", s.get_data()))
                    .collect::<Vec<String>>()
                    .join("")
            );
        }
    }
}
