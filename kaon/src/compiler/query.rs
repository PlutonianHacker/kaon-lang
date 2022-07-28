pub use std::collections::HashMap;
use std::{cell::Cell, num::NonZeroU32, rc::Rc};

use super::{ast::Visibility, typ::Typ};

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct Id(pub NonZeroU32);

#[derive(Debug)]
pub struct Query {
    items: HashMap<Id, MetaItem>,
    entries: HashMap<Id, Entry>,
    generator: Generator,
}

impl Query {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
            entries: HashMap::new(),
            generator: Generator::new(),
        }
    }

    pub fn add_item(&mut self, item: MetaItem) -> Id {
        let id = Id(self.generator.next());
        self.items.insert(id, item);

        id
    }

    pub fn get_item(&self, id: Id) -> Option<&MetaItem> {
        self.items.get(&id)
    }

    pub fn add_entry(&mut self, id: Id, item: Entry) {
        self.entries.insert(id, item);
    }

    pub fn get_entry(&mut self, id: &Id) -> Option<&Entry> {
        self.entries.get(id)
    }
}

#[derive(Debug)]
pub struct MetaItem {
    pub name: String,
    pub typ: Typ,
    pub visibility: Visibility,
}

impl MetaItem {
    pub fn new(name: String, typ: Typ, visibility: Visibility) -> Self {
        Self {
            name,
            typ,
            visibility,
        }
    }
}


#[derive(Debug)]
pub enum Entry {
    Class(ClassItem),
}

#[derive(Debug)]
pub struct ClassItem {
    pub ast: Rc<super::ast::Class>,
}

#[derive(Debug)]
pub struct Generator {
    id: Cell<u32>,
}

impl Generator {
    pub fn new() -> Self {
        Self { id: Cell::new(1) }
    }

    pub fn next(&mut self) -> NonZeroU32 {
        let id = self.id.get();

        let new_id = NonZeroU32::new(id).expect("id cannot be zero");

        *self.id.get_mut() += 1;

        new_id
    }
}
