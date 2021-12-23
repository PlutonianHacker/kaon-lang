use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    Number(f64),
    Boolean(bool),
    Heaped(Rc<RefCell<Data>>),
    String(String),
    Ref(String),
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Data::Number(num) => match num % 1.0 {
                val if val == 0.0 => write!(f, "{}", *num as i64),
                _ => write!(f, "{}", num),
            },
            Data::Boolean(bool) => write!(f, "{}", bool),
            Data::Heaped(_) => write!(f, "Heaped value"),
            Data::String(str) => write!(f, "'{}'", str),
            Data::Ref(str) => write!(f, "{}", str),
        }
    }
}
