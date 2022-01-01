use core::fmt::Debug;
use core::fmt;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub struct Source {
    pub contents: String,
    pub path: PathBuf,
}

impl Source {
    pub fn new(source: &str, path: &PathBuf) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: path.to_path_buf(),
        })
    }

    pub fn contents(source: &str) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from("./main"),
        })
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
