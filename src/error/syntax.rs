use crate::common::Span;
use core::fmt;
use core::fmt::Display;

use crate::error::{Diagnostic, Emitter, Label};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Error {
    // lexer errors
    UnknownEscapeCode(Item),
    InvalidUnicodeEscape(Item),
    UnterminatedString(Item),
    // parser errors
    UnexpectedToken(Item),
    ExpectedToken(Item, Item),
    UnexpectedEOF(Item),
    // typechecker errors
    MismatchType(Item, Item),
    NotInScope(Item),
    MismatchBinOp(Item, Item, Item),
    UnknownType(Item),
    DuplicateIdentifier(Item, Item),
    DuplicateFun(Item, Item),
    UnresolvedIdentifier(Item),
    ExpectedFunction(Item),
    MismatchArgCount(Item, Item, Vec<Item>),
}

impl Error {
    pub fn report(&self) -> Diagnostic {
        match self {
            Error::UnexpectedToken(unexpected_token) => Diagnostic::error()
                .with_code("E0001")
                .with_message("unexpected token")
                .with_labels(vec![Label::primary(unexpected_token.span.clone())
                    .with_message(&format!(
                        "Unexpected token `{}`",
                        unexpected_token.content
                    ))]),
            Error::ExpectedToken(expected_token, unexpected_token) => Diagnostic::error()
                .with_code("E0002")
                .with_message(&format!(
                    "expected token `{}`, found token `{}`",
                    expected_token.content, unexpected_token.content
                ))
                .with_labels(vec![Label::primary(unexpected_token.span.clone())]),
            Error::UnexpectedEOF(item) => Diagnostic::error()
                .with_code("E0003")
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary(item.span.clone()).with_message("unexpected <eof>")
                ]),
            Error::MismatchType(left, right) => Diagnostic::error()
                .with_code("E0003")
                .with_message("mismatched types")
                .with_labels(vec![
                    Label::primary(right.span.clone()).with_message(&format!(
                        "Expected `{}`, found: `{}`",
                        left.content, right.content
                    )),
                    Label::secondary(left.span.clone()).with_message("expected due to this"),
                ]),
            Error::NotInScope(message) => Diagnostic::error()
                .with_code("E0004")
                .with_message(&format!(
                    "cannot find variable `{}` in this scope",
                    message.content
                ))
                .with_labels(vec![
                    Label::primary(message.span.clone()).with_message("not found in this scope")
                ]),
            Error::MismatchBinOp(op, left, right) => Diagnostic::error()
                .with_code("E0005")
                .with_message(&format!(
                    "cannot add `{{{}}}` to `{}`",
                    left.content.clone(),
                    right.content.clone()
                ))
                .with_labels(vec![
                    Label::primary(op.span.clone()),
                    Label::secondary(right.span.clone())
                        .with_message(&format!("{{{}}}", right.content)),
                    Label::secondary(left.span.clone()).with_message(&left.content.to_string()),
                ]),
            Error::UnknownType(message) => {
                Diagnostic::error()
                    .with_code("E0006")
                    .with_message(&format!(
                        "cannot find type {} in this scope",
                        message.content
                    ))
            }
            Error::DuplicateIdentifier(original, duplicate) => Diagnostic::error()
                .with_code("E0007")
                .with_message(&format!("duplicate identifier '{}'", &duplicate.content))
                .with_labels(vec![
                    Label::primary(duplicate.span.clone())
                        .with_message(&format!("duplicate '{}' declared here", duplicate.content)),
                    Label::primary(original.span.clone())
                        .with_message(&format!("original '{}' declared here", original.content)),
                ]),
            Error::DuplicateFun(original, duplicate) => Diagnostic::error()
                .with_code("E0008")
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
                .with_code("E0009")
                .with_message(&format!(
                    "cannot find identifier '{}' in this scope",
                    ident.content
                ))
                .with_labels(vec![
                    Label::primary(ident.span.clone()).with_message("not found in this scope")
                ]),
            Error::UnknownEscapeCode(escape_char) => Diagnostic::error()
                .with_code("E0010")
                .with_message(&format!(
                    "unknown escape character: `{}`",
                    escape_char.content
                ))
                .with_labels(vec![Label::primary(escape_char.span.clone())])
                .with_help(vec![
                    "valid escape characters are \\\", \\\\, \\n, \\r, \\t and \\u{}".into(),
                ]),
            Error::InvalidUnicodeEscape(escape_char) => Diagnostic::error()
                .with_code("E0010")
                .with_message(&format!(
                    "unknown escape character: `{}`",
                    escape_char.content
                ))
                .with_labels(vec![Label::primary(escape_char.span.clone())]),
            Error::UnterminatedString(string) => Diagnostic::error()
                .with_code("E0011")
                .with_message("unterminated string")
                .with_labels(vec![Label::primary(string.span.clone())]),
            Error::ExpectedFunction(typ) => Diagnostic::error()
                .with_code("E0012")
                .with_message(&format!("expected function, found {}", typ.content))
                .with_labels(vec![Label::primary(typ.span.clone())
                    .with_message("call expressions must be functions")]),
            Error::MismatchArgCount(expected_count, actual_count, args) => Diagnostic::error()
                .with_code("E0014")
                .with_message(&format!(
                    "this function take {} arguments, found {} arguments were supplied",
                    expected_count.content, actual_count.content
                ))
                .with_labels(
                    vec![
                        [
                            Label::primary(actual_count.span.clone()).with_message(&format!(
                                "expected {} arguments",
                                expected_count.content
                            )),
                        ]
                        .to_vec(),
                        args.iter()
                            .map(|arg| Label::secondary(arg.span.clone()))
                            .collect::<Vec<Label>>(),
                    ]
                    .concat(),
                ),
        }
    }
}

impl Emitter for Error {}

impl Display for Error {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.emit(&[self.report()]);
        Ok(())
    }
}

/// A wrapper around multiple errors.
pub struct Errors(pub Vec<Diagnostic>);

impl Emitter for Errors {}

impl From<Vec<Error>> for Errors {
    fn from(vec: Vec<Error>) -> Errors {
        Errors(
            vec.to_vec()
                .iter()
                .map(|d| d.report())
                .collect::<Vec<Diagnostic>>(),
        )
    }
}

impl Display for Errors {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.emit(&self.0);
        Ok(())
    }
}
