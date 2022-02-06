use core::fmt;
use core::fmt::Debug;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone, PartialEq, Hash)]
pub struct Source {
    pub contents: String,
    pub path: PathBuf,
}

impl Source {
    pub fn new(source: &str, path: &str) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from(path),
        })
    }

    pub fn contents(source: &str) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from("./main"),
        })
    }

    pub fn from_file(path: &str) -> Result<Rc<Source>, String> {
        match read_to_string(path) {
            Ok(src) => Ok(Source::new(&src, path)),
            Err(err) => Err(err.to_string()),
        }
    }
    pub fn len(&mut self) -> usize {
        self.contents.len()
    }
}

impl Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Source {{ contents: \"...\" path: \"...\"}}")
    }
}
