//! Common data structures shared by the Kaon compiler and VM.

pub mod bytecode;
pub mod disassembler;
pub mod file;
pub mod loader;
pub mod module;
pub mod opcode;
pub mod source;
pub mod span;
pub mod value;
pub mod state;
pub mod string;

pub use bytecode::{Chunk, DebugInfo};
pub use disassembler::Disassembler;
pub use file::{KaonFile, KaonRead, KaonWrite};
pub use loader::Loader;
pub use module::Module;
pub use opcode::Opcode;
pub use source::Source;
pub use span::{Span, Spanned};
pub use value::{
    Captured, Class, Closure, Constructor, External, ExternalData, Function, Instance,
    InstanceMethod, MetaMap, NativeFun, Upvalue, Value, ValueMap,
};

pub use string::ImmutableString;