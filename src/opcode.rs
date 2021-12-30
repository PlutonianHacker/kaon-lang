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

impl From<u8> for Opcode {
    fn from(o: u8) -> Self {
        match o {
            0 => Opcode::Halt,
            1 => Opcode::Const,
            2 => Opcode::Call,
            _ => unreachable!(),
        }
    }
}
