use crate::common::Span;
use core::fmt;
use core::fmt::Display;

use crate::error::{Diagnostic, Label};

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedToken,
    UnterminatedString,
    ExpectedIdentifier,
    MismatchType,
    DuplicateIdentifier,
    UndeclaredFun,
    CompileFail,
    UnknownEscapeCode,
    InvalidUnicodeEscape,
}

#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
    pub span: Span,
    pub kind: ErrorKind,
}

impl SyntaxError {
    pub fn error(kind: ErrorKind, message: &str, span: &Span) -> SyntaxError {
        SyntaxError {
            message: message.to_string(),
            span: span.clone(),
            kind,
        }
    }

    pub fn report(&self) -> Diagnostic {
        match self.kind {
            ErrorKind::UnexpectedToken => Diagnostic::error()
                .with_code("E0001")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::UnterminatedString => Diagnostic::error()
                .with_code("E0002")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::ExpectedIdentifier => Diagnostic::error()
                .with_code("E0003")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::MismatchType => Diagnostic::error()
                .with_code("E0004")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::DuplicateIdentifier => Diagnostic::error()
                .with_code("E0005")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::UndeclaredFun => Diagnostic::error()
                .with_code("E0006")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::UnknownEscapeCode => Diagnostic::error()
                .with_code("E0007")
                .with_message(&self.message)
                .with_labels(vec![
                    Label::primary(self.span.clone()).with_message("unknown character escape")
                ]),
            ErrorKind::InvalidUnicodeEscape => Diagnostic::error()
                .with_code("E0008")
                .with_message(&self.message)
                .with_labels(vec![Label::primary(self.span.clone())]),
            ErrorKind::CompileFail => Diagnostic::error()
                .with_message(&self.message)
                .with_labels(vec![]),
        }
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Display::fmt(&self.span, f)?;
        write!(f, "{}", self.message)
    }
}
