use std::{rc::Rc, collections::BTreeMap};

use crate::compiler::syn::token::{Spacing};

#[derive(Debug, Clone)]
pub enum Object {
    None,
    Ident(String),
    Punct(char, Spacing),
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Array(ArrayObject),
    Composite(CompositeObject),
}
#[derive(Clone)]
pub struct ObjectRef(Rc<Object>);
impl ObjectRef {
    pub fn new(obj: Object) -> Self {
        Self(Rc::new(obj))
    }
}
impl std::fmt::Debug for ObjectRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayObject {
    items: Vec<ObjectRef>,
}
impl ArrayObject {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
        }
    }

    pub fn push(&mut self, obj: ObjectRef) {
        self.items.push(obj);
    }
    pub fn get(&self, idx: usize) -> Option<&ObjectRef> {
        self.items.get(idx)
    }
}

#[derive(Debug, Clone)]
pub struct CompositeObject {
    ty: String,
    attrs: BTreeMap<String, ObjectRef>,
}
impl CompositeObject {
    pub fn new(ty: String) -> Self {
        Self {
            ty,
            attrs: BTreeMap::new(),
        }
    }

    pub fn set_attr(&mut self, name: String, obj: ObjectRef) {
        self.attrs.insert(name, obj);
    }
    pub fn get_attr(&self, name: &str) -> Option<&ObjectRef> {
        self.attrs.get(name)
    }
}
