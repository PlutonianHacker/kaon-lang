use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::fmt;
use std::ops::{Add, Div, Index, Mul, Neg, Rem, Sub};
use std::rc::Rc;

use crate::common::ByteCode;
use crate::core;

use crate::fnv::FnvHashMap;

/// Value type for the Kaon language.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    /// A number
    Number(f64),
    /// A boolean, either true or false
    Boolean(bool),
    /// A string
    String(String),
    /// A list of elements of the same type
    List(Vec<Value>),
    /// A tuple
    Tuple(Vec<Value>),
    /// A map of key, value pairs
    Map(ValueMap),
    /// A native function
    NativeFun(Box<NativeFun>),
    /// A function
    Function(Function),
    /// A closure
    Closure(Closure),
    /// An external data type
    External(External),
    /// An empty type
    Unit,
    /// A nil value
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => match num % 1.0 {
                val if val == 0.0 => write!(f, "{}", *num as i64),
                _ => write!(f, "{}", num),
            },
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::String(str) => write!(f, "{}", str),
            Value::Unit => write!(f, "()"),
            Value::Nil => write!(f, "nil"),
            Value::List(list) => {
                let mut items = vec![];
                for item in list {
                    if let Value::String(val) = item {
                        items.push(format!("\"{}\"", val));
                        continue;
                    }
                    items.push(format!("{}", item))
                }
                write!(f, "[{}]", items.join(", "))
            }
            Value::Tuple(tuple) => {
                let mut items = vec![];
                for item in tuple {
                    if let Value::String(val) = item {
                        items.push(format!("\"{}\"", val));
                        continue;
                    }
                    items.push(format!("{}", item))
                }
                write!(f, "({})", items.join(", "))
            }
            Value::Map(map) => {
                write!(f, "{{ ")?;
                let mut string = "".to_string();
                for pair in &map.data {
                    string += &format!("{}: {}, ", pair.0, pair.1.to_string())[..];
                }
                string.replace_range(string.len() - 2.., "");
                write!(f, "{}", &string)?;
                write!(f, " }}")
            }
            Value::NativeFun(fun) => {
                write!(f, "<native {}>", fun.name)
            }
            Value::Function(fun) => {
                write!(f, "<fun {}>", fun.name)
            }
            Value::Closure(closure) => {
                write!(f, "<fun {}>", closure.function.name)
            }
            Value::External(_) => {
                write!(f, "External Data")
            }
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> <Self as Add<Value>>::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            (Value::String(lhs), Value::String(rhs)) => Value::String(lhs + &rhs),
            (Value::Tuple(mut tuple), Value::Tuple(other)) => {
                tuple.extend(other);
                Value::Tuple(tuple)
            }
            _ => unreachable!("Cannot add non-numbers and non-strings"),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> <Self as Sub<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            return Value::Number(lhs - rhs);
        } else {
            unreachable!()
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Value) -> <Self as Mul<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            return Value::Number(lhs * rhs);
        } else {
            unreachable!()
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Value) -> <Self as Div<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            return Value::Number(lhs / rhs);
        } else {
            unreachable!()
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Value) -> <Self as Rem<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            return Value::Number(lhs % rhs);
        } else {
            unreachable!()
        }
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> <Self as Neg>::Output {
        if let Value::Number(val) = self {
            return Value::Number(-val);
        } else {
            unreachable!()
        }
    }
}

impl Index<f64> for Value {
    type Output = Value;

    fn index(&self, index: f64) -> &Self::Output {
        match self {
            Value::List(list) => &list[index as u32 as usize],
            _ => unreachable!(),
        }
    }
}

/// The Value Map type used in Kaon
#[derive(Debug, Clone)]
pub struct ValueMap {
    data: HashMap<String, Value>,
}

impl ValueMap {
    pub fn new() -> Self {
        ValueMap {
            data: HashMap::new(),
        }
    }

    /// inserts a [NativeFun]
    pub fn insert_fun(&mut self, id: &str, fun: NativeFun) {
        self.data
            .insert(id.to_string(), Value::NativeFun(Box::new(fun)));
    }

    /// inserts a [ValueMap]
    pub fn insert_map(&mut self, id: &str, map: ValueMap) {
        self.data.insert(id.to_string(), Value::Map(map));
    }

    /// inserts a value with a [Value] type
    pub fn insert_constant(&mut self, id: &str, data: Value) {
        self.data.insert(id.to_string(), data);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.data.get(name)
    }
}

impl PartialEq for ValueMap {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl PartialOrd for ValueMap {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

#[derive(Clone, Debug)]
pub enum Captured {
    Local(usize),
    NonLocal(usize),
}

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: ByteCode,
    pub captures: Vec<Captured>,
}

impl Function {
    pub fn new(name: String, arity: usize, chunk: ByteCode, captures: Vec<Captured>) -> Self {
        Function {
            name,
            arity,
            chunk,
            captures,
        }
    }

    pub fn empty() -> Self {
        Self::new("<script>".to_string(), 0, ByteCode::empty(), Vec::new())
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Eq for Function {}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Upvalue {
    pub value: Value,
    pub location: usize,
    pub next: Option<Box<Upvalue>>,
}

impl Upvalue {
    pub fn new(location: usize, value: Value) -> Self {
        Upvalue {
            location,
            value,
            next: None,
        }
    }
}

/*pub enum Upvalue {
    Open(usize),
    Closed(Value),
}*/

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Closure {
    pub function: Rc<Function>,
    pub captures: Vec<Upvalue>,
}

impl Closure {
    pub fn wrap(function: Rc<Function>) -> Self {
        Closure {
            function,
            captures: Vec::new(),
        }
    }

    pub fn empty() -> Self {
        Closure {
            function: Rc::new(Function::empty()),
            captures: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct NativeFun {
    pub name: String,
    pub arity: usize,
    pub fun: core::NativeFun,
    pub varidic: bool,
}

impl NativeFun {
    pub fn new(name: &str, arity: usize, fun: core::NativeFun, varidic: bool) -> Self {
        NativeFun {
            name: name.to_string(),
            arity,
            fun,
            varidic,
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

#[derive(Clone, Debug)]
pub struct MetaMap(FnvHashMap<String, Value>);

impl MetaMap {
    pub fn new() -> Self {
        MetaMap(FnvHashMap::default())
    }

    pub fn insert(&mut self, key: &str, value: Value) {
        self.0.insert(key.to_string(), value);
    }

    pub fn get(&mut self, key: &str) -> Value {
        self.0.get_mut(key.into()).unwrap().clone()
    }
}

pub trait ExternalData {}

impl fmt::Debug for dyn ExternalData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("External Data")
    }
}

#[derive(Clone)]
pub struct External {
    pub data: Rc<RefCell<dyn ExternalData>>,
    pub meta_map: Rc<RefCell<MetaMap>>,
}

impl External {
    pub fn new(data: Rc<RefCell<dyn ExternalData>>, meta_map: Rc<RefCell<MetaMap>>) -> Self {
        Self { data, meta_map }
    }
}

impl fmt::Debug for External {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("External Data")
    }
}

impl PartialOrd for External {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

impl PartialEq for External {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
