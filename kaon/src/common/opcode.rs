#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Opcode {
    /// Load a constant on to the stack.
    Const, 
    True,
    False,
    Nil,
    Unit,
    String,
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
    BitAnd,
    BitOr,
    BitXor,
    DefGlobal,
    SetGlobal,
    GetGlobal,
    LoadLocal,
    SaveLocal,
    LoadUpValue,
    SaveUpValue,
    CloseUpValue,
    /// Set the stack's index pointer backwards by a certain amount.
    Loop,
    /// Unconditional jump.
    Jump,
    /// Jump to a given index pointer if the topmost value on
    /// the stack is truthy.
    JumpIfTrue,
    /// Jump to a given index pointer if the topmost value on
    /// the stack is falsy.
    JumpIfFalse,
    /// Call the topmost value off the stack.
    Call,
    Call0,
    Call1,
    Call2,
    /// Return from the topmost function on the call stack.
    Return,
    /// Pop the topmost value off the stack.
    Pop,
    PopN,
    /// Builds a class from the stack.
    Class,
    /// Build a closure from the stack.
    Closure,
    /// Builds a list from the stack.
    List,
    /// Construct a tuple from the stack.
    Tuple,
    /// Build a map from the stack.
    Map,
    /// Get index into the topmost value on the stack.
    GetIndex,
    /// Set an value at a given index with the supplied value.
    SetIndex,
    /// The get opcode.
    Get,
    /// Set opcode.
    Set,
    /// Import (unused).
    Import,
    /// Halt the vm's exectution.
    Halt,
}

impl From<u8> for Opcode {
    fn from(opcode: u8) -> Opcode {
        unsafe { std::mem::transmute(opcode) }
    }
}
