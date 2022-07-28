use std::{collections::HashMap, rc::Rc};

use crate::{Value, common::{Class, NativeFun, value::RegisterFunction}};

pub struct Module {
    name: Box<str>,
    variables: Vec<Value>,
    symbols: HashMap<String, usize>
}

impl Module {
    pub fn new<T: Into<Box<str>>>(name: T) -> Self {
        Module {
            name: name.into(),
            variables: Vec::new(),
            symbols: HashMap::default(),
        }
    }
    
    pub fn add_const(&mut self, name: &str, value: Value) {
        let index = self.variables.len();

        self.variables.push(value);
        self.symbols.insert(name.to_string(), index);
    }

    pub fn set_const(&mut self, index: usize, value: Value) {
        self.variables[index] = value;
    }

    pub fn get_const(&self, index: usize) -> &Value {
        &self.variables[index]
    }
    
    pub fn get_index(&self, name: &str) -> Option<usize> {
        self.symbols.get(name).copied()
    }

    pub fn add_class(&mut self, name: &str, class: Rc<Class>) {
        self.add_const(name, Value::Class(class));
    } 

    pub fn add_function<A, R, F: RegisterFunction<A, R> + Copy>(&mut self, name: &str, fun: F) {
        let fun = NativeFun::new(name, fun.arity(), fun.to_native_function(), fun.is_varidic());   

        self.add_const(name, Value::NativeFun(Rc::new(fun)));
    }
}
