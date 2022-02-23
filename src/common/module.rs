use super::{Function, Value};

#[derive(Debug)]
pub struct Module {
    pub name: &'static str,
    pub globals: Vec<Value>,
    pub function: Function,
}

impl Module {
    pub fn new(name: &'static str, globals: Vec<Value>, function: Function) -> Self {
        Self {
            name,
            globals,
            function
        }
    }
}
