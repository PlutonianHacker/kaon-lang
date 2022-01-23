use crate::common::{Data, Function};

#[derive(Clone)]
pub struct Frame {
    pub function: Function,
    pub ip: usize,
    pub offset: usize,
}

impl Frame {
    pub fn new(fun: &mut Function, ip: usize, offset: usize) -> Self {
        Frame {
            function: fun.clone(),
            ip,
            offset: offset,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Slot(Data);

impl Slot {
    pub fn new(data: Data) -> Slot {
        Slot(data)
    }

    pub fn get_data(&self) -> &Data {
        &self.0
    }
}

#[derive(Debug)]
pub struct Stack {
    pub stack: Vec<Slot>,
}

impl Stack {
    pub fn new() -> Self {
        Stack { stack: vec![] }
    }

    pub fn pop(&mut self) -> Data {
        self.stack.pop().expect("Stack should not be empty").0
    }

    pub fn push(&mut self, slot: Slot) {
        self.stack.push(slot);
    }

    pub fn peek(&mut self) -> Data {
        self.stack[self.stack.len() - 1].0.clone()
    }

    pub fn set(&mut self, idx: usize, data: Data) {
        self.stack[idx] = Slot::new(data);
    }

    pub fn get(&mut self, idx: usize) -> Slot {
        self.stack[idx].clone()
    }

    pub fn save_local(&mut self, idx: usize, data: Data) {
        self.stack[idx] = Slot::new(data);
    }

    // prints the current stack to stdout
    pub fn debug_stack(&self) {
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
