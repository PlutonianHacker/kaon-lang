use std::fmt::{self, Display};

use crate::vm::Frame;

pub struct Trace {
    error: String,
    frames: Vec<Frame>,
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
        for _ in &mut self.frames.iter().rev() {
            /*if frame.function.name.is_alphanumeric() {
                writeln!(f, "in {}()", &frame.function.name)?;
            } else {
            }*/
        }

        Ok(())
    }
}
