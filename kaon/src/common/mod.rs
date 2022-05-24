//! Common data structures shared by the Kaon compiler and VM.

pub mod args;
pub mod bytecode;
pub mod disassembler;
pub mod file;
pub mod immutable_string;
pub mod opcode;
pub mod source;
pub mod span;
pub mod state;
pub mod value;
mod hash;
pub mod map;

pub use args::{Args, FromArgs, ToArgs, Varidic};
pub use bytecode::{Chunk, DebugInfo};
pub use disassembler::Disassembler;
pub use file::{KaonFile, KaonRead, KaonWrite};
pub use immutable_string::ImmutableString;
pub use opcode::Opcode;
pub use source::Source;
pub use span::{Span, Spanned};
pub use value::{
    BoundMethod, Captured, Class, Closure, Constructor, Function, Instance, NativeFun, Upvalue,
    Value, Named
};
pub use value::{FromValue, ToValue};
pub use map::Map;
