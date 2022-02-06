use crate::common::Span;
use core::fmt;
use core::fmt::Display;

use crate::error::{Diagnostic, Emitter, Label};

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

#[derive(Debug)]
pub struct Item {
    pub content: String,
    pub span: Span,
}

impl Item {
    pub fn new(content: &str, span: Span) -> Self {
        Item {
            content: content.to_string(),
            span,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    MismatchType(Item, Item),
    NotInScope(Item),
    MismatchBinOp(Item, Item, Item),
    UnknownType(Item),
    DuplicateIdentifier(Item, Item),
    DuplicateFun(Item, Item),
    UnresolvedIdentifier(Item),
}

impl Error {
    pub fn report(&self) -> Diagnostic {
        match self {
            Error::MismatchType(left, right) => Diagnostic::error()
                .with_code("E001")
                .with_message("mismatched types")
                .with_labels(vec![
                    Label::primary(right.span.clone()).with_message(&format!(
                        "Expected `{}`, found: `{}`",
                        left.content, right.content
                    )),
                    Label::secondary(left.span.clone()).with_message("expected due to this"),
                ]),
            Error::NotInScope(message) => Diagnostic::error()
                .with_code("E002")
                .with_message(&format!(
                    "cannot find variable `{}` in this scope",
                    message.content
                ))
                .with_labels(vec![
                    Label::primary(message.span.clone()).with_message("not found in this scope")
                ]),
            Error::MismatchBinOp(op, left, right) => Diagnostic::error()
                .with_code("E003")
                .with_message(&format!(
                    "cannot add `{{{}}}` to `{}`",
                    left.content.clone(),
                    right.content.clone()
                ))
                .with_labels(vec![
                    Label::primary(op.span.clone()),
                    Label::secondary(right.span.clone())
                        .with_message(&format!("{{{}}}", right.content)),
                    Label::secondary(left.span.clone()).with_message(&format!("{}", left.content)),
                ]),
            Error::UnknownType(message) => Diagnostic::error().with_code("E004").with_message(
                &format!("cannot find type {} in this scope", message.content),
            ),
            Error::DuplicateIdentifier(original, duplicate) => Diagnostic::error()
                .with_code("E005")
                .with_message(&format!("duplicate identifier '{}'", &duplicate.content))
                .with_labels(vec![
                    Label::primary(duplicate.span.clone())
                        .with_message(&format!("duplicate '{}' declared here", duplicate.content)),
                    Label::primary(original.span.clone())
                        .with_message(&format!("original '{}' declared here", original.content)),
                ]),
            Error::DuplicateFun(original, duplicate) => Diagnostic::error()
                .with_code("E005")
                .with_message(&format!(
                    "function `{}` has already been declared",
                    &duplicate.content
                ))
                .with_labels(vec![
                    Label::secondary(duplicate.span.clone()).with_message(&format!(
                        "function `{}` previously declared here",
                        duplicate.content
                    )),
                    Label::primary(original.span.clone())
                        .with_message(&format!("function `{}` redeclared here", original.content)),
                ])
                .with_help(vec![format!(
                    "`{}` can only be defined once in a scope",
                    original.content
                )]),
            Error::UnresolvedIdentifier(ident) => Diagnostic::error()
                .with_code("E006")
                .with_message(&format!(
                    "cannot find identifier '{}' in this scope",
                    ident.content
                ))
                .with_labels(vec![
                    Label::primary(ident.span.clone()).with_message("not found in this scope")
                ]),
        }
    }
}

impl Display for Error {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        Emitter::emit(vec![self.report()]);
        Ok(())
    }
}
