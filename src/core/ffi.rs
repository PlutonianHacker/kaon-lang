use crate::common::Data;
use crate::vm::VmContext;

use std::collections::HashMap;
use std::{rc::Rc, cell::RefCell};

pub type SharedContext = Rc<RefCell<VmContext>>;

#[derive(Clone)]
pub struct NativeFun(pub Rc<dyn Fn(Rc<RefCell<VmContext>>, Vec<Data>) -> Data>);

impl NativeFun {
    pub fn new(fun: Box<fn(Rc<RefCell<VmContext>>, Vec<Data>) -> Data>) -> Self {
        NativeFun(Rc::new(fun))
    }

    pub fn call(self, vm: Rc<RefCell<VmContext>>, args: Vec<Data>) -> Data {
        (self.0)(vm, args)
    }
}

#[derive(Clone)]
pub struct FFI(HashMap<String, NativeFun>);

impl FFI {
    pub fn new() -> Self {
        FFI(HashMap::new())
    }

    pub fn add(&mut self, ident: &str, fun: NativeFun) {
        self.0.insert(ident.to_string(), fun);
    }

    pub fn get(&mut self, ident: &str) -> Option<&NativeFun> {
        self.0.get(ident)
    }
}
