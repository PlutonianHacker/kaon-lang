#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    /// stop the program
    Halt = 0,
    /// load a new value onto the stack
    Const = 1,
    /// call a function
    Call = 2,
}
/*
impl From<u8> for Opcode {
    fn from(opcode: Opcode) -> Self {
        mem::replace(u8, opcode)
    }
}*/
