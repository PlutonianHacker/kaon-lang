use crate::stack::Data;

#[derive(Debug, Clone)]
pub enum Opcode {
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Equal,
    NotEqual,
    GToEq,
    LToEq,
    Gt,
    Lt,
    Not,
    SetGlobal,
    //GetGlobal,
    Halt,
}

impl From<Opcode> for u8 {
    fn from(opcode: Opcode) -> u8 {
        match opcode {
            Opcode::Const => 0,
            Opcode::Add => 1,
            Opcode::Sub => 2,
            Opcode::Mul => 3,
            Opcode::Div => 4,
            Opcode::Negate => 5,
            Opcode::Equal => 6,
            Opcode::NotEqual => 7,
            Opcode::GToEq => 8,
            Opcode::LToEq => 9,
            Opcode::Gt => 10,
            Opcode::Lt => 11,
            Opcode::Not => 12,
            Opcode::SetGlobal => 13,
            Opcode::Halt => 14,
        }
    }
}

impl From<u8> for Opcode {
    fn from(val: u8) -> Opcode {
        match val {
            0 => Opcode::Const,
            1 => Opcode::Add,
            2 => Opcode::Sub,
            3 => Opcode::Mul,
            4 => Opcode::Div,
            5 => Opcode::Negate,
            6 => Opcode::Equal,
            7 => Opcode::NotEqual,
            8 => Opcode::GToEq,
            9 => Opcode::LToEq,
            10 => Opcode::Gt,
            11 => Opcode::Lt,
            12 => Opcode::Not,
            13 => Opcode::SetGlobal,
            14 => Opcode::Halt,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Data>,
}
