use std::fmt;
use std::fmt::Display;
use std::rc::Rc;

use crate::common::Source;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Span {
    pub source: Rc<Source>,
    pub start: usize,
    pub length: usize,
}

impl Span {
    pub fn new(start: usize, length: usize, source: &Rc<Source>) -> Self {
        Span {
            start,
            length,
            source: source.clone(),
        }
    }

    pub fn empty() -> Self {
        Span {
            start: 0,
            length: 0,
            source: Source::contents(""),
        }
    }

    pub fn end(&self) -> usize {
        self.start + self.length
    }

    pub fn combine(a: &Span, b: &Span) -> Span {
        let start = a.start.min(b.start);
        let end = a.end().max(b.end());

        let length = end - start; 

        Span::new(start, length, &a.source)
    }

    pub fn lines(string: &str) -> Vec<String> {
        string.split("\n").map(|line| line.to_string()).collect()
    }

    pub fn line_index(source: &str, offset: usize) -> (usize, usize) {
        let lines = Span::lines(&source[..offset]);
        let line = lines.len() - 1;
        let col = lines.last().unwrap().chars().count();
        return (line, col);
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = &self.source.as_ref().contents;
        let lines = Span::lines(source);

        let (start_line, start_col) = Span::line_index(&source, self.start);
        let (end_line, _) = Span::line_index(&source, self.start + self.length - 1);

        let readable_start_line = (start_line + 1).to_string();
        let readable_start_col = (start_col + 1).to_string();
        let readable_end_line = (end_line + 1).to_string();
        let padding = readable_end_line.len();

        let location = format!(
            " In {}:{}:{}",
            self.source.path.to_string_lossy(),
            readable_start_line,
            readable_start_col
        );

        let sperator = format!("{} | ", " ".repeat(padding));
        let line = format!("{} | {}", readable_start_line, &lines[end_line]);
        let span = format!(
            "{} | {}{}",
            " ".repeat(padding),
            " ".repeat(start_col),
            "^".repeat(self.length.max(1).min(lines[end_line].len()))
        );

        writeln!(f, "{}", location)?;
        writeln!(f, "{}", sperator)?;
        writeln!(f, "{}", line)?;
        writeln!(f, "{}", span)
    }
}

pub struct Spanned<T> {
    pub node: T,
    pub source: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, source: Span) -> Spanned<T> {
        Spanned { node, source }
    }
}
