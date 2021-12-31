use std::path::PathBuf;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
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
