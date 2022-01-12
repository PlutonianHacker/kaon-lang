use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::rc::Rc;

use crate::core;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Data {
    Number(f64),
    Boolean(bool),
    Heaped(Rc<RefCell<Data>>),
    String(String),
    Ref(String),
    Unit,
    List(Vec<Data>),
    NativeFun(Box<NativeFun>),
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
            Data::String(str) => write!(f, "{}", str),
            Data::Ref(str) => write!(f, "{}", str),
            Data::Unit => write!(f, "()"),
            Data::List(list) => {
                let mut items = vec![];
                for item in list {
                    if let Data::String(val) = item {
                        items.push(format!("\"{}\"", val));
                        continue;
                    }
                    items.push(format!("{}", item))
                }
                write!(f, "[{}]", items.join(", "))
            }
            Data::NativeFun(fun) => {
                write!(f, "<native {}>", fun.name)
            }
        }
    }
}

impl Add for Data {
    type Output = Data;

    fn add(self, rhs: Data) -> <Self as Add<Data>>::Output {
        if let (Data::Number(lhs), Data::Number(rhs)) = (self, rhs) {
            return Data::Number(lhs + rhs);
        } else {
            unreachable!()
        }
    }
}

impl Sub for Data {
    type Output = Data;

    fn sub(self, rhs: Data) -> <Self as Sub<Data>>::Output {
        if let (Data::Number(lhs), Data::Number(rhs)) = (self, rhs) {
            return Data::Number(lhs - rhs);
        } else {
            unreachable!()
        }
    }
}

impl Mul for Data {
    type Output = Data;

    fn mul(self, rhs: Data) -> <Self as Mul<Data>>::Output {
        if let (Data::Number(lhs), Data::Number(rhs)) = (self, rhs) {
            return Data::Number(lhs * rhs);
        } else {
            unreachable!()
        }
    }
}

impl Div for Data {
    type Output = Data;

    fn div(self, rhs: Data) -> <Self as Div<Data>>::Output {
        if let (Data::Number(lhs), Data::Number(rhs)) = (self, rhs) {
            return Data::Number(lhs / rhs);
        } else {
            unreachable!()
        }
    }
}

impl Rem for Data {
    type Output = Data;

    fn rem(self, rhs: Data) -> <Self as Rem<Data>>::Output {
        if let (Data::Number(lhs), Data::Number(rhs)) = (self, rhs) {
            return Data::Number(lhs % rhs);
        } else {
            unreachable!()
        }
    }
}

impl Neg for Data {
    type Output = Data;
    fn neg(self) -> <Self as Neg>::Output {
        if let Data::Number(val) = self {
            return Data::Number(-val);
        } else {
            unreachable!()
        }
    }
}

#[derive(Clone)]
pub struct NativeFun {
    pub name: String,
    pub arity: usize,
    pub fun: core::NativeFun,
}

impl NativeFun {
    pub fn new(name: &str, arity: usize, fun: core::NativeFun) -> Self {
        NativeFun {
            name: name.to_string(),
            arity,
            fun,
        }
    }
}

impl fmt::Debug for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for NativeFun {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for NativeFun {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NativeFun {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for NativeFun {}

#[cfg(test)]
mod test {
    use crate::core::ffi_core;
    use crate::data::{Data, NativeFun};

    #[test]
    fn native_fun() {
        let mut ffi = ffi_core();
        let _ = Data::NativeFun(Box::new(NativeFun::new(
            "println",
            1,
            ffi.get("println").unwrap().clone(),
        )));
    }
}
