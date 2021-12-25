use crate::data::Data;

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
    LoadGlobal,
    SaveGlobal,
    LoadLocal,
    SaveLocal,
    Jump,
    Jeq,
    Print,
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
            Opcode::Mod => 5,
            Opcode::Negate => 6,
            Opcode::Equal => 7,
            Opcode::NotEqual => 8,
            Opcode::Gte => 9,
            Opcode::Lte => 10,
            Opcode::Gt => 11,
            Opcode::Lt => 12,
            Opcode::Not => 13,
            Opcode::LoadGlobal => 14,
            Opcode::SaveGlobal => 15,
            Opcode::LoadLocal => 16,
            Opcode::SaveLocal => 17,
            Opcode::Jump => 18,
            Opcode::Jeq => 19,
            Opcode::Print => 20,
            Opcode::Halt => 21,
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
            5 => Opcode::Mod,
            6 => Opcode::Negate,
            7 => Opcode::Equal,
            8 => Opcode::NotEqual,
            9 => Opcode::Gte,
            10 => Opcode::Lte,
            11 => Opcode::Gt,
            12 => Opcode::Lt,
            13 => Opcode::Not,
            14 => Opcode::LoadGlobal,
            15 => Opcode::SaveGlobal,
            16 => Opcode::LoadLocal,
            17 => Opcode::SaveGlobal,
            18 => Opcode::Jump,
            19 => Opcode::Jeq,
            20 => Opcode::Print,
            21 => Opcode::Halt,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Data>,
}
