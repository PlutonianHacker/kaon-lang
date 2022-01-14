use crate::span::Span;
use core::fmt;
use core::fmt::Display;

#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
    pub span: Span,
}

impl SyntaxError {
    pub fn error(message: &str, span: &Span) -> SyntaxError {
        SyntaxError {
            message: message.to_string(),
            span: span.clone(),
        }
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Display::fmt(&self.span, f)?;
        write!(f, "{}", self.message)
    }
}
