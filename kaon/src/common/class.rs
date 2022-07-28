use std::{
    any::TypeId,
    cell::{Ref, RefCell},
    cmp::Ordering,
    collections::HashMap,
    fmt,
    rc::Rc,
};

use crate::Value;

use super::{
    hash,
    value::{CallableFunction, Fun, RegisterFunction},
    Instance, NativeFun, ToValue,
};

/// A class data structure.
#[derive(Default)]
pub struct Class {
    /// The class's name.
    pub name: Box<str>,
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

    pub fn fields(&self) -> Ref<Vec<(Box<str>, Value)>> {
        self.fields.borrow()
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
