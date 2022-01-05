use crate::data::Data;

#[derive(Debug, Clone)]
pub struct Slot(Data);

impl Slot {
    pub fn new(data: Data) -> Slot {
        Slot(data)
    }
}

#[derive(Debug)]
pub struct Stack {
    stack: Vec<Slot>,
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
        //let len = self.stack.len() - 1;
        self.stack[idx] = Slot::new(data);
    }
}
