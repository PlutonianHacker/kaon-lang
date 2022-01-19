use crate::common::Data;

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Data>,
}

impl ByteCode {
    pub fn empty() -> Self {
        ByteCode {
            opcodes: vec![],
            constants: vec![],
        }
    }

    pub fn add_constant(&mut self, constant: Data) -> usize {
        let index = self.constants.len();
        self.constants.push(constant);
        index
    }
}
