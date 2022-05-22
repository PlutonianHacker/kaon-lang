use std::any::TypeId;
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

use super::{hash, ImmutableString, ToArgs, Varidic};

/// Value type for the Kaon language.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Value {
    /// A 64-bit floating pointer number
    Float(f64),
    /// An interger value
    Integer(i64),
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

    pub fn as_closure(&self) -> Option<Rc<Closure>> {
        if let Value::Closure(closure) = self {
            Some(closure.clone())
        } else {
            None
        }
    }

    pub fn as_class(&self) -> Option<&Rc<Class>> {
        if let Self::Class(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Float(num) => write!(f, "{num}"),
            Value::Integer(num) => write!(f, "{num}"),
            Value::Boolean(bool) => write!(f, "{bool}"),
            Value::String(str) => write!(f, "{str}"),
            Value::Unit => write!(f, "()"),
            Value::Nil => write!(f, "nil"),
            Value::List(list) => {
                let mut items = vec![];
                for item in RefCell::borrow(&list.0).iter() {
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
                write!(f, "<fun {}>", closure.name())
            }
            Value::Class(class) => write!(f, "{class}"),
            Value::Instance(instance) => write!(f, "{instance}"),
            Value::Method(method) => write!(f, "{method}"),
            Value::Constructor(init) => write!(f, "{init}"),
        }
    }
}

/// A generic function type.
pub type Fun = dyn Fn(&mut Vm, Vec<Value>) -> Value;

/// A trait for defining how a function gets called.
pub trait Callable<Args> {
    fn call(&self, vm: &mut Vm, args: Args) -> Value;
}

#[derive(Clone)]
pub enum CallableFunction {
    /// A native function.
    Native(Rc<NativeFun>),
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
    fn arity(self) -> Box<[TypeId]>;

    fn is_varidic(&self) -> bool {
        false
    }

    fn to_native_function(self) -> Rc<Fun>;
}

impl<F: Fn() -> R + 'static, R: ToValue> RegisterFunction<(), R> for F {
    fn to_native_function(self) -> Rc<Fun> {
        Rc::new(Box::new(move |_vm: &mut Vm, _args: Vec<Value>| {
            self().to_value()
        }))
    }

    fn arity(self) -> Box<[TypeId]> {
        vec![].into_boxed_slice()
    }
}

impl<F: Fn(Value) -> R + 'static, R: ToValue> RegisterFunction<Value, R> for F {
    fn to_native_function(self) -> Rc<Fun> {
        Rc::new(Box::new(move |_vm: &mut Vm, mut args: Vec<Value>| {
            self(args.pop().unwrap()).to_value()
        }))
    }

    fn arity(self) -> Box<[TypeId]> {
        vec![TypeId::of::<Value>()].into_boxed_slice()
    }
}

impl<F: Fn(&mut V) -> R + 'static, V: FromValue, R: ToValue> RegisterFunction<&mut V, R> for F {
    fn to_native_function(self) -> Rc<Fun> {
        Rc::new(Box::new(move |_vm: &mut Vm, mut args: Vec<Value>| {
            self(&mut V::from_value(&args.pop().unwrap()).unwrap()).to_value()
        }))
    }

    fn arity(self) -> Box<[TypeId]> {
        vec![TypeId::of::<Value>()].into_boxed_slice()
    }
}

impl<F: Fn(&mut Vm) -> R + 'static, R: ToValue>
    RegisterFunction<&mut Vm, R> for F
{
    fn to_native_function(self) -> Rc<Fun> {
        Rc::new(Box::new(move |vm: &mut Vm, _args: Vec<Value>| {
            self(vm).to_value()
        }))
    }

    fn is_varidic(&self) -> bool {
        false
    }

    fn arity(self) -> Box<[TypeId]> {
        vec![TypeId::of::<Vm>()].into_boxed_slice()
    }
}

impl<F: Fn(&mut Vm, Varidic<T>) -> R + 'static, R: ToValue, T: FromValue>
    RegisterFunction<(&mut Vm, Varidic<T>), R> for F
{
    fn to_native_function(self) -> Rc<Fun> {
        Rc::new(Box::new(move |vm: &mut Vm, args: Vec<Value>| {
            self(vm, Varidic::new_from_iter::<Value>(args.iter())).to_value()
        }))
    }

    fn is_varidic(&self) -> bool {
        true
    }

    fn arity(self) -> Box<[TypeId]> {
        vec![TypeId::of::<Value>()].into_boxed_slice()
    }
}

macro_rules! register_function {
    () => {};
    ($param1:ident $($param:ident)*)  => {
        register_function!($($param)*);

        impl<FN: Fn(&mut REF, $param1, $($param,)*) -> RET + 'static, $param1: FromValue + 'static, $($param: FromValue + 'static,)* RET: ToValue, REF: FromValue + 'static> RegisterFunction<(&mut REF, $param1, $($param,)*), RET> for FN {
            #[allow(non_snake_case)]
            fn to_native_function(self) -> Rc<Fun> {
                Rc::new(Box::new(move |_vm: &mut Vm, mut args: Vec<Value>| {
                    let mut iter = args.iter();

                    let $param1 = $param1::from_value(iter.next().expect("Oh no. It's broken")).unwrap();
                    $(let $param = $param::from_value(iter.next().expect("Oh no. It's broken.")).unwrap();)*

                    self(&mut REF::from_value(&args.pop().unwrap()).unwrap(), $param1, $($param,)*).to_value()
                }))
            }

            fn arity(self) -> Box<[TypeId]> {
                vec![TypeId::of::<REF>(), TypeId::of::<$param1>(), $(TypeId::of::<$param>(),)*].into_boxed_slice()
            }
        }

        impl<FN: Fn(&mut Vm, $param1, $($param,)*) -> RET + 'static, $param1: FromValue + 'static, $($param: FromValue + 'static,)* RET: ToValue> RegisterFunction<(&mut Vm, $param1, $($param,)*), RET> for FN {
            #[allow(non_snake_case)]
            fn to_native_function(self) -> Rc<Fun> {
                Rc::new(Box::new(move |vm: &mut Vm, args: Vec<Value>| {
                    let mut iter = args.iter();

                    let $param1 = $param1::from_value(iter.next().expect("Oh no. It's broken")).unwrap();
                    $(let $param = $param::from_value(iter.next().expect("Oh no. It's broken.")).unwrap();)*

                    self(vm, $param1, $($param,)*).to_value()
                }))
            }

            fn arity(self) -> Box<[TypeId]> {
                vec![TypeId::of::<$param1>(), $(TypeId::of::<$param>(),)*].into_boxed_slice()
            }
        }

        impl<FN: Fn(&mut Vm, $param1, $($param,)* Varidic<VAL>) -> RET + 'static, $param1: FromValue + 'static, $($param: FromValue + 'static,)* VAL: FromValue, RET: ToValue> RegisterFunction<(&mut Vm, $param1, $($param,)* Varidic<VAL>), RET> for FN {
            #[allow(non_snake_case)]
            fn to_native_function(self) -> Rc<Fun> {
                Rc::new(Box::new(move |vm: &mut Vm, args: Vec<Value>| {
                    let mut iter = args.iter();

                    let $param1 = $param1::from_value(iter.next().expect("Oh no. It's broken")).unwrap();
                    $(let $param = $param::from_value(iter.next().expect("Oh no. It's broken.")).unwrap();)*

                    self(vm, $param1, $($param,)* Varidic::new_from_iter::<VAL>(iter)).to_value()
                }))
            }

            fn is_varidic(&self) -> bool {
                true
            }

            fn arity(self) -> Box<[TypeId]> {
                vec![TypeId::of::<$param1>(), $(TypeId::of::<$param>(),)*].into_boxed_slice()
            }
        }

        impl<FN: Fn($param1, $($param,)* Varidic<VAL>) -> RET + 'static, $param1: FromValue + 'static, $($param: FromValue + 'static,)* VAL: FromValue, RET: ToValue> RegisterFunction<($param1, $($param,)* Varidic<VAL>), RET> for FN {
            #[allow(non_snake_case)]
            fn to_native_function(self) -> Rc<Fun> {
                Rc::new(Box::new(move |_vm: &mut Vm, args: Vec<Value>| {
                    let mut iter = args.iter();

                    let $param1 = $param1::from_value(iter.next().expect("Oh no. It's broken")).unwrap();
                    $(let $param = $param::from_value(iter.next().expect("Oh no. It's broken.")).unwrap();)*

                    self($param1, $($param,)* Varidic::new_from_iter::<VAL>(iter)).to_value()
                }))
            }

            fn is_varidic(&self) -> bool {
                true
            }

            fn arity(self) -> Box<[TypeId]> {
                vec![TypeId::of::<$param1>(), $(TypeId::of::<$param>(),)*].into_boxed_slice()
            }
        }
    };
}

register_function!(A B C D E F G H I J K L M N O P Q R S T U V W X);

/// A class data structure.
#[derive(Default)]
pub struct Class {
    /// The class's name.
    pub name: Box<str>,
    //pub inits: RefCell<HashMap<Box<str>, CallableFunction>>,
    pub methods: RefCell<HashMap<u64, CallableFunction>>,
    /// The class's fields.
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
            fields: RefCell::new(Vec::new()),
            methods: RefCell::new(HashMap::new()),
        }
    }

    /// Insert a [CallableFunction] into the class's methods table.
    pub fn add_function<S: Into<Box<str>> + Copy>(
        &self,
        hash: u64,
        name: S,
        arity: Box<[TypeId]>,
        fun: Rc<Fun>,
        is_varidic: bool,
    ) {
        self.methods.borrow_mut().insert(
            hash,
            CallableFunction::Native(Rc::new(NativeFun::new(name, arity, fun, is_varidic))),
        );
    }

    /// Register a rust function as a method of this class.
    pub fn register_method<S: Into<Box<str>> + Copy, A, R, C: RegisterFunction<A, R> + Copy>(
        &self,
        name: S,
        fun: C,
    ) {
        let arity = fun.arity();
        let is_varidic = fun.is_varidic();
        let fun = fun.to_native_function();
        let hash = hash::calculate_hash(&name.into(), hash::METHOD);

        self.add_function(hash, name, arity, fun, is_varidic);
    }

    /// Register a rust function as a static class function.
    pub fn register_static<S: Into<Box<str>> + Copy, A, R, C: RegisterFunction<A, R> + Copy>(
        &self,
        name: S,
        fun: C,
    ) {
        let arity = fun.arity();
        let is_varidic = fun.is_varidic();
        let fun = fun.to_native_function();
        let hash = hash::calculate_hash(&name.into(), hash::STATIC);

        self.add_function(hash, name, arity, fun, is_varidic);
    }

    /// Regisiter an constructor function.
    pub fn register_init<S: Into<Box<str>> + Copy, A, R, C: RegisterFunction<A, R> + Copy>(
        &self,
        name: S,
        fun: C,
    ) {
        let arity = fun.arity();
        let is_varidic = fun.is_varidic();
        let fun = fun.to_native_function();
        let hash = hash::calculate_hash(&name.into(), hash::INIT);

        self.add_function(hash, name, arity, fun, is_varidic);
    }

    /// Add a field to this class.
    pub fn add_field<S: Into<Box<str>>, V: ToValue>(&self, name: S, init: V) {
        self.fields
            .borrow_mut()
            .push((name.into(), init.to_value()));
    }

    /// Add a constructor function to this class.
    pub fn add_method<S: Into<Box<str>>>(&self, name: S, fun: CallableFunction) {
        let hash = hash::calculate_hash(&name.into(), hash::METHOD);

        self.methods.borrow_mut().insert(hash, fun);
    }

    /// Add a constructor function to this class.
    pub fn add_static<S: Into<Box<str>>>(&self, name: S, fun: CallableFunction) {
        let hash = hash::calculate_hash(&name.into(), hash::STATIC);

        self.methods.borrow_mut().insert(hash, fun);
    }

    /// Add a constructor function to this class.
    pub fn add_init<S: Into<Box<str>>>(&self, name: S, fun: CallableFunction) {
        let hash = hash::calculate_hash(&name.into(), hash::INIT);

        self.methods.borrow_mut().insert(hash, fun);
    }

    /// Get a method.
    pub fn get_method(&self, name: &str) -> Option<CallableFunction> {
        let hash = hash::calculate_hash(name, hash::METHOD);

        self.methods.borrow().get(&hash).cloned()
    }
    
    /// Get a static function.
    pub fn get_static(&self, name: &str) -> Option<CallableFunction> {
        let hash = hash::calculate_hash(name, hash::STATIC);

        self.methods.borrow().get(&hash).cloned()
    }

    /// Get a constructor function.
    pub fn get_init(&self, name: &str) -> Option<CallableFunction> {
        let hash = hash::calculate_hash(name, hash::INIT);

        self.methods.borrow().get(&hash).cloned()
    }

    /// Create a new `Rc<Instance>` from this class.
    pub fn instance(self: Rc<Self>) -> Rc<Instance> {
        Instance::new(self)
    }
}

impl ToValue for Rc<Class> {
    fn to_value(self) -> Value {
        Value::Class(self)
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("<class {}>", self.name))
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
        let name = name.into();
        let hash = hash::calculate_hash(&name, hash::METHOD);

        let methods = &receiver.class.as_ref().methods.borrow();
        let method = methods.get(&hash).unwrap();

        BoundMethod::new(receiver.clone().to_value(), method.clone())
    }

    pub fn builtin<S: Into<Box<str>>>(receiver: Value, name: S, class: Rc<Class>) -> BoundMethod {
        let methods = class.as_ref().borrow().methods.borrow();
        let hash = hash::calculate_hash(&name.into(), hash::METHOD);

        let method = methods.get(&hash).unwrap();

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

impl fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.function {
            CallableFunction::Native(fun) => f.write_fmt(format_args!("<method {}>", fun.name)),
            CallableFunction::Function(fun) => {
                f.write_fmt(format_args!("<method {}>", fun.function.name))
            }
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

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.function {
            CallableFunction::Native(fun) => f.write_fmt(format_args!("<method {}>", fun.name)),
            CallableFunction::Function(fun) => {
                f.write_fmt(format_args!("<method {}>", fun.function.name))
            }
        }
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
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::Tuple(_tuple), Value::Tuple(_other)) => {
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
        if let (Value::Float(lhs), Value::Float(rhs)) = (self, rhs) {
            Value::Float(lhs - rhs)
        } else {
            unreachable!()
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Value) -> <Self as Mul<Value>>::Output {
        if let (Value::Float(lhs), Value::Float(rhs)) = (self, rhs) {
            Value::Float(lhs * rhs)
        } else {
            unreachable!()
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Value) -> <Self as Div<Value>>::Output {
        if let (Value::Float(lhs), Value::Float(rhs)) = (self, rhs) {
            Value::Float(lhs / rhs)
        } else {
            unreachable!()
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Value) -> <Self as Rem<Value>>::Output {
        if let (Value::Float(lhs), Value::Float(rhs)) = (self, rhs) {
            Value::Float(lhs % rhs)
        } else {
            unreachable!()
        }
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> <Self as Neg>::Output {
        if let Value::Float(val) = self {
            Value::Float(-val)
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
    pub name: Box<str>,
    pub param_typs: Box<[TypeId]>,
    pub fun: Rc<Fun>,
    pub is_varidic: bool,
}

impl NativeFun {
    pub fn new<S: Into<Box<str>>>(
        name: S,
        param_typs: Box<[TypeId]>,
        fun: Rc<Fun>,
        is_varidic: bool,
    ) -> Self {
        NativeFun {
            name: name.into(),
            param_typs,
            fun,
            is_varidic,
        }
    }

    pub fn arity(&self) -> usize {
        self.param_typs.len()
    }

    pub fn call(&self, vm: &mut Vm, args: Vec<Value>) -> Value {
        (self.fun)(vm, args)
    }
}

impl fmt::Debug for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_struct("NativeFun")
            .field("name", &self.name)
            .field("params", &self.param_typs)
            .field("is_varidic", &self.is_varidic)
            .finish()
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

macro_rules! impl_into_value {
    ($typ:ty, $T:ident, $as: ty) => {
        impl ToValue for $typ {
            fn to_value(self) -> Value {
                Value::$T(self as $as)
            }
        }
    };
    ($typ:ty, $T:ident) => {
        impl ToValue for $typ {
            fn to_value(self) -> Value {
                Value::$T(self.into())
            }
        }
    };
}

impl_into_value!(&str, String);
impl_into_value!(String, String);
impl_into_value!(&String, String);
impl_into_value!(Box<str>, String);
impl_into_value!(&mut str, String);
impl_into_value!(i64, Integer);
impl_into_value!(i8, Integer, i64);
impl_into_value!(i16, Integer, i64);
impl_into_value!(i32, Integer, i64);
impl_into_value!(i128, Integer, i64);
impl_into_value!(isize, Integer, i64);
impl_into_value!(u8, Integer, i64);
impl_into_value!(u16, Integer, i64);
impl_into_value!(u32, Integer, i64);
impl_into_value!(u64, Integer, i64);
impl_into_value!(u128, Integer, i64);
impl_into_value!(usize, Integer, i64);
impl_into_value!(f64, Float);
impl_into_value!(f32, Float, f64);
impl_into_value!(bool, Boolean);
impl_into_value!(ImmutableString, String);

impl From<()> for Value {
    fn from(_: ()) -> Value {
        Value::Nil
    }
}

impl FromValue for Value {
    fn from_value(value: &Value) -> Result<Self, String> {
        Ok(value.to_owned())
    }
}

macro_rules! impl_from_value {
    ($typ:ty, ($T:pat => $e:expr)) => {
        impl FromValue for $typ {
            fn from_value(value: &Value) -> Result<$typ, String> {
                match value {
                    $T => $e,
                    value => Err(format!("cannot coerce type from value {}", value)),
                }
            }
        }
    };
}

impl_from_value!(f64, (Value::Float(v) => Ok(*v)));
impl_from_value!(f32, (Value::Float(v) => Ok(*v as f32)));
impl_from_value!(i32, (Value::Integer(v) => Ok(*v as i32)));
impl_from_value!(i64, (Value::Integer(v) => Ok(*v as i64)));
impl_from_value!(u32, (Value::Integer(v) => Ok(*v as u32)));
impl_from_value!(String, (Value::String(v) => Ok(v.to_string())));
impl_from_value!(bool, (Value::Boolean(v) => Ok(*v)));
impl_from_value!(Rc<Class>, (Value::Class(v) => Ok(v.to_owned())));
impl_from_value!(Rc<Instance>, (Value::Instance(v) => Ok(v.clone())));
impl_from_value!(ImmutableString, (Value::String(str) => Ok(str.clone())));

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
