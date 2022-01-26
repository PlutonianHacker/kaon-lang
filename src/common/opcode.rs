#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Opcode {
    Const,
    True,
    False,
    Nil,
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
    LoadUpValue,
    SaveUpValue,
    CloseUpValue,
    Jump,
    JumpIfTrue,
    JumpIfFalse,
    Print,
    Call,
    Closure,
    Return,
    Del,
    List,
    Index,
    Loop,
    Halt,
}

impl From<u8> for Opcode {
    fn from(opcode: u8) -> Opcode {
        unsafe { std::mem::transmute(opcode) }
    }
}
