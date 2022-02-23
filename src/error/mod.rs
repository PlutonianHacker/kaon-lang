//! Error diagnostics for the Kaon language.

pub mod diagnostic;
pub mod renderer;
pub mod syntax;

use crate::error::renderer::Renderer;

pub use crate::error::diagnostic::Diagnostic;
pub use crate::error::diagnostic::{Label, LabelStyle, Severity};
pub use crate::error::syntax::{Error, Item, Errors};
use termcolor::{ColorChoice, StandardStream};

/// A trait for emitting a [Diagnostic].
pub trait Emitter {
    fn emit(&self, errors: &[Diagnostic]) {
        let mut writer = StandardStream::stderr(ColorChoice::Always);

        let mut renderer = Renderer::new(&mut writer);
        for error in errors {
            renderer.render(error.clone()).unwrap();
        }
    }
}
