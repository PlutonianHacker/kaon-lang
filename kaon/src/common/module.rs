#![allow(dead_code)]

use smallvec::SmallVec;

const INLINE: usize = 32;

/// The name of an item or value.
#[derive(Debug)]
pub struct Item {
    content: SmallVec<[u8; INLINE]>,
}

impl Item {
    /// Construct a new empty item.
    pub const fn new() -> Self {
        Self {
            content: SmallVec::new_const(),
        }
    }

    /// Construct a new item from an Iterator.
    pub fn with_item<I>(item: I) -> Self
    where
        I: IntoIterator,
        I::Item: IntoComponent,
    {
        let mut content = SmallVec::new();

        for c in item {
            c.write_component(&mut content);
        }

        Self { content }
    }

    /// Push a new component onto item.
    pub fn push<C>(&mut self, c: C)
    where
        C: IntoComponent,
    {
        c.write_component(&mut self.content);
    }

    /// Convert self into a vec of [Component]s
    pub fn as_vec(&self) {}
}

#[derive(Debug)]
pub enum Component {
    Module(Box<str>),
    Str(Box<str>),
    Id(usize),
}

impl Component {}

impl IntoComponent for Component {}

impl IntoComponent for &&str {
    fn write_component(&self, content: &mut SmallVec<[u8; INLINE]>) {
        content.extend_from_slice(" ".as_bytes());
        content.extend_from_slice(self.as_bytes());
    }
}

pub trait IntoComponent: Sized {
    fn write_component(&self, _content: &mut SmallVec<[u8; INLINE]>) {}
}



#[derive(Debug, Default)]
pub struct Module {
    pub name: String,
    //pub constants: Consts,
}

impl Module {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            //fun_declarations: Vec::new(),
            //constants: FnvHashMap::default(),
            //chunk: ByteCode::empty(),
        }
    }
}
