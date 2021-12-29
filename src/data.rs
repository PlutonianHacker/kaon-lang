use std::fmt::Error;
use std::fmt::{Debug, Formatter};

#[derive(Clone, PartialEq)]
pub enum Data {
    Number(f64),
    String(String),
    Boolean(bool),
    Unit,
    Nil,
}

impl Debug for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match &*self {
            Data::Number(val) => write!(f, "{}", val),
            Data::String(val) => write!(f, "{}", val),
            Data::Boolean(val) => write!(f, "{}", val),
            Data::Unit => write!(f, "()"),
            Data::Nil => write!(f, "nil"),
        }
    }
}
