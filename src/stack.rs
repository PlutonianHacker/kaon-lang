use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Data {
    Number(f64),
    Boolean(bool),
    Heaped(Rc<RefCell<Data>>),
    String(String),
    Ref(String),
}

impl Data {}

#[derive(Debug, Clone)]
pub struct Slot(Data);

impl Slot {
    pub fn new(data: Data) -> Slot {
        Slot(data)
    }

    pub fn extract(&mut self) -> Data {
        self.0.clone()
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
}
