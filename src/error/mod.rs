pub mod diagnostic;
pub mod error;
pub mod renderer;

pub use crate::error::diagnostic::Diagnostic;
use crate::error::renderer::Renderer;
use termcolor::{ColorChoice, StandardStream};

pub use crate::error::diagnostic::{Label, LabelStyle, Severity};
pub use crate::error::error::{SyntaxError, ErrorKind};

pub struct Emitter {}

impl Emitter {
    pub fn emit(errors: Vec<Diagnostic>) {
        let mut writer = StandardStream::stderr(ColorChoice::Always);

        let mut renderer = Renderer::new(&mut writer);
        for error in errors {
            renderer.render(error).unwrap();
        }
    }
}
