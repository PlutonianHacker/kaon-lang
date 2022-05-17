use core::fmt;
use core::fmt::Debug;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::rc::Rc;

/// Tracks the source code used within the Kaon language.
#[derive(Clone, PartialEq, Hash, Default)]
pub struct Source {
    pub contents: String,
    pub path: PathBuf,
}

impl Source {
    /// Create a new reference counted [Source] from a string.
    pub fn new(source: &str, path: &str) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from(path),
        })
    }

    /// Create a new [Source] without specifing the file path.
    pub fn contents(source: &str) -> Rc<Source> {
        Rc::new(Source {
            contents: source.to_string(),
            path: PathBuf::from("./main"),
        })
    }

    /// Read a file from the provided path and return it as a [Source]. 
    pub fn from_file(path: &str) -> Result<Rc<Source>, String> {
        match read_to_string(path) {
            Ok(src) => Ok(Source::new(&src, path)),
            Err(err) => Err(err.to_string()),
        }
    }

    /// Merge the contents of one [Source] into another.
    pub fn merge(&mut self, other: Rc<Source>) {
        self.contents += &other.as_ref().contents.clone();
    }

    pub fn len(&mut self) -> usize {
        self.contents.len()
    }

    pub fn is_empty(&mut self) -> bool {
        self.contents.is_empty()
    }
}

impl Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Source {{ contents: \"...\" path: \"...\"}}")
    }
}
