use crate::common::Value;
use crate::runtime::VmContext;

use std::{rc::Rc, cell::RefCell};

pub type SharedContext = Rc<RefCell<VmContext>>;

type InnerFun = dyn Fn(Rc<RefCell<VmContext>>, Vec<Value>) -> Value;

#[derive(Clone)]
pub struct NativeFun(pub Rc<InnerFun>);

impl NativeFun {
    pub fn new(fun: Box<InnerFun>) -> Self {
        NativeFun(Rc::new(fun))
    }

    pub fn call(self, vm: Rc<RefCell<VmContext>>, args: Vec<Value>) -> Value {
        (self.0)(vm, args)
    }
}
