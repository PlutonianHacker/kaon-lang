use std::{collections::HashMap, rc::Rc};

use crate::Value;

pub struct Pool {
    length: u16,
    entries: HashMap<Rc<Value>, u16>,
    indexes: Vec<Rc<Value>>,
}

impl Pool {
    pub fn new() -> Self {
        Pool {
            length: 0,
            entries: HashMap::new(),
            indexes: Vec::new(),
        }
    }

    pub fn with_capacity(cap: u16) -> Self {
        Pool {
            length: 0,
            entries: HashMap::with_capacity(cap.into()),
            indexes: Vec::with_capacity(cap.into()),
        }
    }

    #[inline]
    pub fn length(&self) -> u16 {
        self.length
    }

    
}


