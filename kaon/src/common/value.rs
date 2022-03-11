use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::Rc;

use smallvec::SmallVec;

use crate::common::ByteCode;
use crate::core;

use crate::fnv::FnvHashMap;

/// Value type for the Kaon language.
#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    /// A number
    Number(f64),
    /// A boolean, either true or false
    Boolean(bool),
    /// A string
    String(Rc<String>),
    /// A list of elements of the same type
    List(ValueList),
    /// A tuple
    Tuple(ValueTuple),
    /// A map of key, value pairs
    Map(Rc<ValueMap>),
    /// A native function
    NativeFun(Rc<NativeFun>),
    /// A function
    Function(Rc<Function>),
    /// A closure
    Closure(Rc<RefCell<Closure>>),
    /// A class declaration
    Class(Rc<RefCell<Class>>),
    /// An instance of a class
    Instance(Rc<Instance>),
    /// A class constructor
    Constructor(Rc<Constructor>),
    /// A instance method
    InstanceMethod(Rc<InstanceMethod>),
    /// An external data type
    External(Rc<External>),
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
                for item in list.0.borrow().iter() {
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
                for item in tuple.0.iter() {
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
                    string += &format!("{}: {}, ", pair.0, pair.1)[..];
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
                write!(f, "<fun {}>", closure.as_ref().borrow().function.name)
            }
            Value::Class(class) => {
                write!(f, "<class {}>", class.borrow().name)
            }
            Value::Instance(instance) => {
                write!(f, "<instance {}>", instance.class_name())
            }
            Value::InstanceMethod(method) => {
                write!(f, "<method {}>", method.name)
            }
            Value::Constructor(constructor) => {
                write!(f, "<constructor {}>", constructor.name)
            }
            Value::External(_) => {
                write!(f, "External Data")
            }
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Number(val) => Self::Number(*val),
            Self::Boolean(val) => Self::Boolean(*val),
            Self::String(val) => Self::String(val.clone()),
            Self::List(val) => Self::List(val.clone()),
            Self::Tuple(val) => Self::Tuple(val.clone()),
            Self::Map(val) => Self::Map(val.clone()),
            Self::NativeFun(val) => Self::NativeFun(val.clone()),
            Self::Function(val) => Self::Function(val.clone()),
            Self::Closure(val) => Self::Closure(val.clone()),
            Self::Class(val) => Self::Class(val.clone()),
            Self::Instance(val) => Self::Instance(val.clone()),
            Self::Constructor(val) => Self::Constructor(val.clone()),
            Self::InstanceMethod(val) => Self::InstanceMethod(val.clone()),
            Self::External(val) => Self::External(val.clone()),
            Self::Unit => Self::Unit,
            Self::Nil => Self::Nil,
        }
    }
}

impl From<Value> for bool {
    fn from(val: Value) -> Self {
        match val {
            Value::Boolean(val) => val,
            _ => unreachable!(),
        }
    }
}

impl From<Value> for f64 {
    fn from(val: Value) -> Self {
        match val {
            Value::Number(val) => val,
            _ => unreachable!(),
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> <Self as Add<Value>>::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            //(Value::String(lhs), Value::String(rhs)) => Value::String(lhs.to_string() + rhs.),
            (Value::Tuple(_tuple), Value::Tuple(_other)) => {
                //tuple.0.extend(other.0);
                //tuple.0.extend(other.0.iter());

                //Value::Tuple(tuple)
                todo!("This needs to be fixed.")
            }
            _ => unreachable!("Cannot add non-numbers and non-strings"),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> <Self as Sub<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            Value::Number(lhs - rhs)
        } else {
            unreachable!()
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Value) -> <Self as Mul<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            Value::Number(lhs * rhs)
        } else {
            unreachable!()
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Value) -> <Self as Div<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            Value::Number(lhs / rhs)
        } else {
            unreachable!()
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Value) -> <Self as Rem<Value>>::Output {
        if let (Value::Number(lhs), Value::Number(rhs)) = (self, rhs) {
            Value::Number(lhs % rhs)
        } else {
            unreachable!()
        }
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> <Self as Neg>::Output {
        if let Value::Number(val) = self {
            Value::Number(-val)
        } else {
            unreachable!()
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Self::Boolean(true) => Value::Boolean(false),
            Self::Boolean(false) => Value::Boolean(true),
            // this should never be reached
            _ => Value::Boolean(false),
        }
    }
}

/// The Value Map type used in Kaon
#[derive(Debug, Clone, Default)]
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
            .insert(id.to_string(), Value::NativeFun(Rc::new(fun)));
    }

    /// inserts a [ValueMap]
    pub fn insert_map(&mut self, id: &str, map: ValueMap) {
        self.data.insert(id.to_string(), Value::Map(Rc::new(map)));
    }

    /// inserts a value with a [Value] type
    pub fn insert_constant(&mut self, id: &str, data: Value) {
        self.data.insert(id.to_string(), data);
    }

    pub fn get(&self, name: &str) -> Result<&Value, String> {
        self.data
            .get(name)
            .ok_or_else(|| format!("cannot find member `{name}`"))
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

#[derive(Clone, Debug)]
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

    pub fn script() -> Self {
        Self::new("<script>".to_string(), 0, ByteCode::empty(), Vec::new())
    }

    pub(crate) fn default() -> Function {
        Self::script()
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
    pub value: Rc<Value>,
    /// The next upvalue in the list, possibly none.
    pub next: Option<Rc<Upvalue>>,
    /// position in the stack?
    pub position: usize,
    pub closed: Value,
}

impl Upvalue {
    pub fn new(value: Rc<Value>, next: Option<Rc<Upvalue>>, position: usize) -> Self {
        Upvalue {
            value,
            next,
            position,
            closed: Value::Nil,
        }
    }

    pub fn next(&self) -> Option<Upvalue> {
        if let Some(val) = &self.next {
            Some(val.as_ref().clone())
        } else {
            None
        }
    }
}

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
            function: Rc::new(Function::script()),
            captures: Vec::new(),
        }
    }

    pub fn capture(&mut self, index: usize, value: Value) {
        self.captures[index].value = Rc::new(value);
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

#[derive(Clone, Debug, Default)]
pub struct MetaMap(FnvHashMap<String, Value>);

impl MetaMap {
    pub fn new() -> Self {
        MetaMap(FnvHashMap::default())
    }

    pub fn insert(&mut self, key: &str, value: Value) {
        self.0.insert(key.to_string(), value);
    }

    pub fn get(&mut self, key: &str) -> Value {
        self.0.get_mut(key).unwrap().clone()
    }
}

/// A class declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    /// The name of the class
    pub name: String,
    /// class methods
    pub methods: FnvHashMap<String, Value>,
    /// class constructors
    pub constructors: FnvHashMap<String, Constructor>,
    /// class fields
    pub fields: usize, //FnvHashMap<String, Value>,
    /// the parent that this class inherits from, if any.
    pub super_class: Option<Rc<Class>>,
}

impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Class {
    pub fn new(name: String, fields: usize) -> Self {
        Self {
            name,
            methods: FnvHashMap::default(),
            constructors: FnvHashMap::default(),
            fields,
            super_class: None,
        }
    }

    pub fn add_method(&mut self, name: String, method: Value) {
        self.methods.insert(name, method);
    }

    pub fn add_constructor(&mut self, name: String, method: Constructor) {
        self.constructors.insert(name, method);
    }

    pub fn _add_field(&mut self, _name: String, _value: Value) {
        //self.fields.insert(name, value);
    }
}

/// An instance of a [Class]
#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<RefCell<Class>>,
    pub fields: Vec<Value>,
}

impl Instance {
    pub fn new(class: Rc<RefCell<Class>>, cap: usize) -> Self {
        Self {
            class,
            fields: vec![Value::Nil; cap],
        }
    }

    pub fn class_name(&self) -> String {
        self.class.as_ref().borrow().name.to_string()
    }

    pub fn get_field(&self, index: usize) -> &Value {
        &self.fields[index]
    }

    pub fn set_field(&mut self, index: usize, value: Value) {
        self.fields[index] = value;
    }
}

impl PartialOrd for Instance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.class_name().partial_cmp(&other.class_name())
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        self.class_name() == other.class_name()
    }
}

/// A class constructor
#[derive(Debug, Clone)]
pub struct Constructor {
    // constructor name
    pub name: String,
    pub initilizer: Rc<RefCell<Closure>>,
    pub receiver: Value,
}

impl Constructor {
    pub fn new(name: String, initilizer: Rc<RefCell<Closure>>, receiver: Value) -> Self {
        Self {
            name,
            initilizer,
            receiver,
        }
    }
}

impl PartialEq for Constructor {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.initilizer == other.initilizer
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

/// An instance method
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceMethod {
    name: String,
}

impl PartialOrd for InstanceMethod {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
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

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ValueList(pub Rc<RefCell<SmallVec<[Value; 4]>>>);

impl ValueList {
    #[inline]
    pub fn new() -> Self {
        ValueList(Rc::new(RefCell::new(SmallVec::<[Value; 4]>::new())))
    }

    pub fn from_vec(vec: &[Value]) -> Self {
        ValueList(Rc::new(RefCell::new(SmallVec::<[Value; 4]>::from(vec))))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.borrow().is_empty()
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ValueTuple(pub Rc<SmallVec<[Value; 4]>>);

impl ValueTuple {
    #[inline]
    pub fn new() -> Self {
        Self(Rc::new(SmallVec::<[Value; 4]>::new()))
    }
}
