use crate::common::Span;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

impl Display for LabelStyle {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LabelStyle::Primary => write!(f, "^"),
            LabelStyle::Secondary => write!(f, "-"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Label {
    pub message: String,
    pub span: Span,
    pub style: LabelStyle,
}

impl Label {
    pub fn new(style: LabelStyle, span: Span) -> Self {
        Label {
            message: String::new(),
            span,
            style,
        }
    }

    pub fn primary(span: Span) -> Self {
        Label::new(LabelStyle::Primary, span)
    }

    pub fn secondary(span: Span) -> Self {
        Label::new(LabelStyle::Secondary, span)
    }

    pub fn with_message(mut self, message: &str) -> Self {
        self.message = message.to_string();
        self
    }
}

#[derive(Clone)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub code: Option<String>,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(severity: Severity) -> Self {
        Diagnostic {
            severity: severity,
            message: String::new(),
            code: None,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn error() -> Self {
        Diagnostic::new(Severity::Error)
    }

    pub fn warning() -> Self {
        Diagnostic::new(Severity::Warning)
    }

    pub fn with_message(mut self, message: &str) -> Diagnostic {
        self.message.push_str(message);
        self
    }

    pub fn with_code(mut self, code: &str) -> Diagnostic {
        self.code = Some(code.to_string());
        self
    }

    pub fn with_labels(mut self, mut labels: Vec<Label>) -> Diagnostic {
        self.labels.append(&mut labels);
        self
    }

    pub fn with_help(mut self, mut message: Vec<String>) -> Diagnostic {
        self.notes.append(&mut message);
        self
    }
}
