//! Kaon language VM and runtime

pub mod stack;
pub mod stdio;
pub mod trace;
#[allow(clippy::module_inception)]
pub mod vm;

pub use stack::{Frame, Stack};
pub use stdio::{KaonStderr, KaonStdin, KaonStdout};
pub use trace::Trace;
pub use vm::{Vm, VmSettings, VmContext};
