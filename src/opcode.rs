use crate::data::Data;

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
    LoadGlobal,
    SaveGlobal,
    LoadLocal,
    SaveLocal,
    Jump,
    Jeq,
    Print,
    FFICall,
    Del,
    List,
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
            Opcode::And => 14,
            Opcode::Or => 15,
            Opcode::LoadGlobal => 16,
            Opcode::SaveGlobal => 17,
            Opcode::LoadLocal => 18,
            Opcode::SaveLocal => 19,// <---
            Opcode::Jump => 20,
            Opcode::Jeq => 21,
            Opcode::Print => 22,
            Opcode::FFICall => 23,
            Opcode::Del => 24,
            Opcode::List => 25,
            Opcode::Halt => 26,
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
            14 => Opcode::And,
            15 => Opcode::Or,
            16 => Opcode::LoadGlobal,
            17 => Opcode::SaveGlobal,
            18 => Opcode::LoadLocal,
            19 => Opcode::SaveLocal,
            20 => Opcode::Jump,
            21 => Opcode::Jeq,
            22 => Opcode::Print,
            23 => Opcode::FFICall,
            24 => Opcode::Del,
            25 => Opcode::List,
            26 => Opcode::Halt,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Data>,
}
