use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::cmp::{Ord, Ordering};
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::Rc;

use smallvec::SmallVec;

use crate::common::Chunk;
use crate::runtime::Vm;

use super::{ImmutableString, ToArgs};

/// Value type for the Kaon language.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Value {
    /// A number
    Number(f64),
    /// A boolean, either true or false
    Boolean(bool),
    /// A string
    String(ImmutableString),
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
    Closure(Rc<Closure>),
    /// A class declaration
    Class(Rc<Class>),
    /// An instance of a class
    Instance(Rc<Instance>),
    /// A class constructor
    Constructor(Rc<Constructor>),
    /// A instance method
    Method(Rc<BoundMethod>),
    /// An empty type
    Unit,
    /// A nil value
    Nil,
}

impl Value {
    pub const TRUE: Value = Value::Boolean(true);
    pub const FALSE: Value = Value::Boolean(false);

    pub(crate) fn as_closure(&self) -> Option<Rc<Closure>> {
        if let Value::Closure(closure) = self {
            Some(closure.clone())
        } else {
            None
        }
    }
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
                for item in  RefCell::borrow(&list.0).iter() {
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
                write!(f, "<fun {}>", closure.as_ref().name())
            }
            Value::Class(class) => {
                write!(f, "<class {}>", class.name)
            }
            Value::Instance(instance) => {
                write!(f, "{}", instance)
            }
            Value::Method(_) => {
                write!(f, "<method>")
            }
            Value::Constructor(_) => {
                write!(f, "<constructor>")
            }
        }
    }
}

/// A generic function type.
pub type Fun = dyn Fn(Vec<Value>) -> Value;

/// A trait for defining how a function gets called.
pub trait Callable<Args> {
    fn call(&self, vm: &mut Vm, args: Args) -> Value;
}

#[derive(Clone)]
pub enum CallableFunction {
    /// A native function.
    Native(Rc<Fun>),
    /// A script function.
    Function(Rc<Closure>),
}

impl Debug for CallableFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Native(_) => f.write_str("Native"),
            Self::Function(_) => f.write_str("Function"),
        }
    }
}

pub trait RegisterFunction<Args, Return> {
    fn to_function(self) -> CallableFunction;
}

impl<F: Fn(Value) -> R + 'static, R: ToValue> RegisterFunction<Value, R> for F {
    fn to_function(self) -> CallableFunction {
        CallableFunction::Native(Rc::new(Box::new(move |mut args: Vec<Value>| {
            self(args.pop().unwrap()).to_value()
        })))
    }
}

impl<F: Fn(&mut V) -> R + 'static, V: FromValue, R: ToValue> RegisterFunction<&mut V, R> for F {
    fn to_function(self) -> CallableFunction {
        CallableFunction::Native(Rc::new(Box::new(move |mut args: Vec<Value>| {
            self(&mut V::from_value(&args.pop().unwrap()).unwrap()).to_value()
        })))
    }
}

macro_rules! register_function {
    () => {};
    ($param1:ident $($param:ident)*)  => {
        register_function!($($param)*);

        impl<FN: Fn(&mut REF, $param1, $($param,)*) -> RET + 'static, $param1: FromValue, $($param: FromValue,)* RET: ToValue, REF: FromValue> RegisterFunction<(&mut REF, $param1, $($param,)*), RET> for FN {
            #[allow(non_snake_case)]
            fn to_function(self) -> CallableFunction {
                CallableFunction::Native(Rc::new(Box::new(move |mut args: Vec<Value>| {
                    let mut iter = args.iter();

                    let $param1 = $param1::from_value(iter.next().expect("Oh no. It's broken")).unwrap();
                    $(let $param = $param::from_value(iter.next().expect("Oh no. It's broken.")).unwrap();)*

                    self(&mut REF::from_value(&args.pop().unwrap()).unwrap(), $param1, $($param,)*).to_value()
                })))
            }
        }
    };
}

register_function!(A B C D E F G H I J K L M N O P Q R S T U V W X);

#[derive(Default)]
pub struct Class {
    /// The class's name.
    pub name: Box<str>,
    pub methods: RefCell<HashMap<Box<str>, CallableFunction>>,
    pub inits: RefCell<HashMap<Box<str>, CallableFunction>>,
    fields: RefCell<Vec<(Box<str>, Value)>>,
}

impl Class {
    /// Create a new reference counted class.
    pub fn new<S: Into<Box<str>>>(name: S) -> Rc<Self> {
        Rc::new(Self::raw(name))
    }

    /// Create a plain-old, non-reference counted class.
    pub fn raw<S: Into<Box<str>>>(name: S) -> Self {
        Self {
            name: name.into(),
            methods: RefCell::new(HashMap::new()),
            inits: RefCell::new(HashMap::new()),
            fields: RefCell::new(Vec::new()),
        }
    }

    pub fn add_method<S: Into<Box<str>> + Copy>(&self, name: S, fun: CallableFunction) {
        self.methods.borrow_mut().insert(name.into(), fun);
    }

    pub fn register_method<S: Into<Box<str>> + Copy, A, R, C: RegisterFunction<A, R>>(
        &self,
        name: S,
        fun: C,
    ) {
        self.methods
            .borrow_mut()
            .insert(name.into(), fun.to_function());
    }

    pub fn add_field<S: Into<Box<str>>, V: ToValue>(&self, name: S, init: V) {
        self.fields
            .borrow_mut()
            .push((name.into(), init.to_value()));
    }

    pub fn add_init<S: Into<Box<str>>>(&self, name: S, fun: CallableFunction) {
        self.inits.borrow_mut().insert(name.into(), fun);
    }

    /// Create a new `Rc<Instance>` from this class.
    pub fn instance(self: Rc<Self>) -> Rc<Instance> {
        Instance::new(self)
    }

    pub fn instance_with() -> Rc<Instance> {
        todo!()
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field("fields", &self.fields)
            .field("methods", &self.methods)
            .finish()
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

/// An instance of a [Class].
#[derive(Debug, PartialEq)]
pub struct Instance {
    /// A reference to the instance's class.
    pub class: Rc<Class>,
    /// The instance's fields.
    pub fields: RefCell<HashMap<Box<str>, Value>>,
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Rc<Self> {
        let mut fields = HashMap::new();

        for (id, value) in &*class.fields.borrow() {
            fields.insert(id.clone(), value.clone());
        }

        Rc::new(Self {
            class,
            fields: RefCell::new(fields),
        })
    }

    /// Bind a method with the given name and call it immediately.
    pub fn invoke<S: Into<Box<str>>>(vm: &mut Vm, receiver: Rc<Instance>, name: S) -> Value {
        let bound = Instance::bind(receiver, name);

        bound.call(vm, ())
    }

    /// Bind a method to an instance.
    pub fn bind<S: Into<Box<str>>>(receiver: Rc<Instance>, name: S) -> BoundMethod {
        let methods = &receiver.class.as_ref().methods.borrow();
        let method = methods.get(&name.into()).unwrap();

        BoundMethod::new(receiver.clone().to_value(), method.clone())
    }

    pub fn builtin<S: Into<Box<str>>>(receiver: Value, name: S, class: Rc<Class>) -> BoundMethod {
        let methods = class.as_ref().borrow().methods.borrow();
        let method = methods.get(&name.into()).unwrap();

        BoundMethod::new(receiver, method.clone())
    }

    /// Get a mutable reference to the instance's fields.
    pub fn fields_mut(&self) -> RefMut<'_, HashMap<Box<str>, Value>> {
        self.fields.borrow_mut()
    }

    /// Get a reference to the instance's fields.
    pub fn fields(&self) -> Ref<'_, HashMap<Box<str>, Value>> {
        self.fields.borrow()
    }

    pub fn field<S: Into<Box<str>>>(&self, _name: S) -> Value {
        let fields = self.fields();
        fields.get(&_name.into()).unwrap().clone()
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("<instance {}>", self.class.as_ref().name))
    }
}

impl PartialOrd for Instance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.class.partial_cmp(&other.class)
    }
}

pub struct BoundMethod {
    pub receiver: Value,
    pub function: CallableFunction,
}

impl BoundMethod {
    pub fn new<V: ToValue>(receiver: V, function: CallableFunction) -> Self {
        Self {
            receiver: receiver.to_value(),
            function,
        }
    }
}

impl fmt::Debug for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BoundMethod")
            .field("reciever", &self.receiver)
            .field("function", &"Rc<CallableFunction>")
            .finish()
    }
}

impl PartialEq for BoundMethod {
    fn eq(&self, other: &Self) -> bool {
        self.receiver == other.receiver
    }
}

impl PartialOrd for BoundMethod {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

impl<A: ToArgs> Callable<A> for BoundMethod {
    fn call(&self, _vm: &mut Vm, _args: A) -> Value {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub class: Rc<Class>,
    pub function: CallableFunction,
}

impl PartialEq for Constructor {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

impl Constructor {
    pub fn new(class: Rc<Class>, function: CallableFunction) -> Self {
        Self { class, function }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> <Self as Add<Value>>::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            (Value::Tuple(_tuple), Value::Tuple(_other)) => {
                //tuple.0.extend(other.0);
                //tuple.0.extend(other.0.iter());

                //Value::Tuple(tuple)
                todo!("This needs to be fixed.")
            }
            (Value::String(string), Value::String(other)) => Value::String(string + other),
            (x, y) => unreachable!("Cannot add non-numbers and non-strings {} {}", x, y),
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

impl BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Boolean(val), Value::Boolean(rhs)) => Value::Boolean(*val | *rhs),
            _ => todo!(),
        }
    }
}

impl BitOr for Value {
    type Output = Value;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Boolean(val), Value::Boolean(rhs)) => Value::Boolean(*val | *rhs),
            _ => todo!(),
        }
    }
}

impl BitXor for Value {
    type Output = Value;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Boolean(val), Value::Boolean(rhs)) => Value::Boolean(*val | *rhs),
            _ => todo!(),
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
    pub chunk: Chunk,
    pub captures: Vec<Captured>,
}

impl Function {
    pub fn new(name: String, arity: usize, chunk: Chunk, captures: Vec<Captured>) -> Self {
        Function {
            name,
            arity,
            chunk,
            captures,
        }
    }

    pub fn script() -> Self {
        Self::new("script".to_string(), 0, Chunk::empty(), Vec::new())
    }

    pub(crate) fn default() -> Function {
        Self::script()
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
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
    pub captures: RefCell<Vec<Upvalue>>,
}

impl Closure {
    pub fn wrap(function: Rc<Function>) -> Self {
        Closure {
            function,
            captures: RefCell::new(Vec::new()),
        }
    }

    pub fn empty() -> Self {
        Closure {
            function: Rc::new(Function::script()),
            captures: RefCell::new(Vec::new()),
        }
    }

    pub fn capture(&mut self, index: usize, value: Value) {
        self.captures.borrow_mut()[index].value = Rc::new(value);
    }

    /// Helper method for getting the function's name.
    pub fn name(&self) -> &str {
        &self.function.name
    }
}

pub type ExternalFun = fn(&mut Vm, Vec<Value>) -> Value;

#[derive(Clone)]
pub struct NativeFun {
    pub name: String,
    pub arity: usize,
    pub varidic: bool,
    pub fun: ExternalFun, //dyn FnMut(&mut Vm, Vec<Value>) -> Value,
}

impl NativeFun {
    pub fn new(name: &str, arity: usize, fun: ExternalFun) -> Self {
        NativeFun {
            name: name.to_string(),
            arity,
            fun,
            varidic: false,
        }
    }

    /// Create a new native function with varidic arguments.
    pub fn varidic(name: &str, arity: usize, fun: ExternalFun) -> Self {
        NativeFun {
            name: name.to_string(),
            arity,
            fun,
            varidic: true,
        }
    }

    pub fn call(&self, vm: &mut Vm, args: Vec<Value>) -> Value {
        (self.fun)(vm, args)
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
        RefCell::borrow(&self.0).len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        RefCell::borrow(&self.0).is_empty()
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

pub trait FromValue: Sized {
    fn from_value(value: &Value) -> Result<Self, String>;
}

pub trait ToValue {
    fn to_value(self) -> Value;
}

impl ToValue for () {
    fn to_value(self) -> Value {
        Value::Nil
    }
}

impl ToValue for Value {
    fn to_value(self) -> Value {
        self
    }
}

impl ToValue for Rc<Instance> {
    fn to_value(self) -> Value {
        Value::Instance(self)
    }
}

macro_rules! impl_to_value {
    ($r_typ:ty, $k_typ:ident) => {
        impl ToValue for $r_typ {
            fn to_value(self) -> Value {
                Value::$k_typ(self.into())
            }
        }
    };
    ($r_typ:ty as $k_typ:ident) => {
        impl ToValue for $r_typ {
            fn to_value(self) -> Value {
                Value::$k_typ(self.into())
            }
        }
    };
}

impl_to_value!(&str, String);
impl_to_value!(Box<str>, String);
impl_to_value!(String, String);
impl_to_value!(&String, String);
impl_to_value!(bool, Boolean);
impl_to_value!(i8 as Number);
impl_to_value!(i16 as Number);
impl_to_value!(i32 as Number);
impl_to_value!(u8 as Number);
impl_to_value!(u16 as Number);
impl_to_value!(u32 as Number);
impl_to_value!(f32 as Number);
impl_to_value!(f64 as Number);

#[cfg(test)]
mod test {
    use std::mem;

    use super::{ToValue, Value};

    #[test]
    fn test_to_value() {
        let value = true;

        assert_eq!(Value::TRUE, value.to_value());
    }

    #[test]
    fn test_size_of_value() {
        assert_eq!(16, mem::size_of::<Value>());
    }
}
