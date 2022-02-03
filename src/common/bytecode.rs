use crate::common::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Value>,
    pub identifiers: HashMap<String, usize>,
}

impl ByteCode {
    pub fn empty() -> Self {
        ByteCode {
            opcodes: vec![],
            constants: vec![],
            identifiers: HashMap::new(),
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(constant);
        index
    }

    pub fn identifier(&mut self, value: String) -> usize {
        /*match self.identifiers.get(&value) {
            Some(index) => *index,
            None => {
                let index = self.constants.len();
                self.add_constant(Value::String(value.clone()));
                self.identifiers.insert(value, index);

                index
            }
        }*/
        self.add_constant(Value::String(value))
    }
}
