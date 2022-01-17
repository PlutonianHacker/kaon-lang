use crate::common::data::Data;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Opcode {
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Negate,
    Equal,
    NotEqual,
    Gte,
    Lte,
    Gt,
    Lt,
    Not,
    Or,
    And,
    DefGlobal,
    SetGlobal,
    GetGlobal,
    LoadLocal,
    SaveLocal,
    Jump,
    Jeq,
    Print,
    Call,
    Del,
    List,
    Loop,
    Halt,
}

impl From<u8> for Opcode {
    fn from(opcode: u8) -> Opcode {
        unsafe { std::mem::transmute(opcode) }
    }
}

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
}