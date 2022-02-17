use std::fmt::{self, Display};

use crate::{common::Span, vm::Frame};

/// The stack trace.
///
/// Used for post-mortem debugging.
pub struct Trace {
    /// the error message
    pub error: String,
    /// the call stack
    pub frames: Vec<Frame>,
}

impl Trace {
    pub fn new(error: &str, frames: Vec<Frame>) -> Self {
        Trace {
            error: error.to_string(),
            frames,
        }
    }
}

impl Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.error)?;

        for (_, frame) in &mut self.frames.iter().rev().enumerate() {
            let span = frame
                .closure
                .function
                .chunk
                .debug_info
                .source_map
                .last()
                .unwrap()
                .1
                .clone();

            let source = &span.source.as_ref().contents;

            let (start_line, start_col) = Span::line_index(source, span.start);
            let readable_start_line = (start_line + 1).to_string();
            let readable_start_col = (start_col + 1).to_string();

            writeln!(
                f,
                "{} in {} ({}:{}:{})",
                " ".repeat(3),
                frame.closure.function.name,
                span.source.path.to_string_lossy(),
                readable_start_line,
                readable_start_col
            )?;
        }

        Ok(())
    }
}
