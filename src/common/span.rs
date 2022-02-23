use std::rc::Rc;

use crate::common::Source;

#[derive(Debug, PartialEq, Hash)]
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
        string.split('\n').map(|line| line.to_string()).collect()
    }

    pub fn line_index(source: &str, offset: usize) -> (usize, usize) {
        let lines = Span::lines(&source[..offset]);
        let line = lines.len() - 1;
        let col = lines.last().unwrap().chars().count();
        (line, col)
    }
}

impl Clone for Span {
    fn clone(&self) -> Self {
        Self {
            source: self.source.clone(),
            start: self.start,
            length: self.length,
        }
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
