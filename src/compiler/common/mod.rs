use std::{rc::Rc, collections::HashMap};

pub mod span;

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
}
impl ConstantValue {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(x) => Some(*x),
            _ => None,
        }
    }
    pub fn as_int(&self) -> Option<i32> {
        match self {
            Self::Int(x) => Some(*x),
            _ => None,
        }
    }
    pub fn as_float(&self) -> Option<f32> {
        match self {
            Self::Int(x) => Some(*x as f32),
            Self::Float(x) => Some(*x),
            _ => None,
        }
    }

    pub fn to_words(&self) -> Vec<u32> {
        match self {
            Self::Bool(_) => panic!("boolean value cannot be represented in words"),
            Self::Int(x) => vec![*x as u32],
            Self::Float(x) => vec![x.to_bits()],
            Self::String(x) => {
                let mut words = vec![];
                let mut bytes = x.as_bytes().iter().copied().collect::<Vec<_>>();
                bytes.push(0);

                for c in bytes.chunks(4) {
                    let mut c2 = [0u8; 4];
                    c2[0..c.len()].copy_from_slice(c);
                    let c = u32::from_le_bytes(c2);
                    words.push(c);
                }
                words
            }
        }
    }

    pub fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x + y),
            (Self::Float(x), Self::Float(y)) => Self::Float(x + y),
            _ => panic!("cannot add {:?} and {:?}", self, other),
        }
    }
    pub fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x - y),
            (Self::Float(x), Self::Float(y)) => Self::Float(x - y),
            _ => panic!("cannot sub {:?} and {:?}", self, other),
        }
    }
    pub fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x * y),
            (Self::Float(x), Self::Float(y)) => Self::Float(x * y),
            _ => panic!("cannot mul {:?} and {:?}", self, other),
        }
    }
    pub fn div(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x / y),
            (Self::Float(x), Self::Float(y)) => Self::Float(x / y),
            _ => panic!("cannot div {:?} and {:?}", self, other),
        }
    }
    pub fn rem(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x % y),
            (Self::Float(x), Self::Float(y)) => Self::Float(x % y),
            _ => panic!("cannot rem {:?} and {:?}", self, other),
        }
    }
    pub fn neg(&self) -> Self {
        match self {
            Self::Int(x) => Self::Int(-x),
            Self::Float(x) => Self::Float(-x),
            _ => panic!("cannot neg {:?}", self),
        }
    }
    pub fn not(&self) -> Self {
        match self {
            Self::Bool(x) => Self::Bool(!x),
            _ => panic!("cannot not {:?}", self),
        }
    }
    pub fn xor(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(x), Self::Bool(y)) => Self::Bool(*x ^ *y),
            _ => panic!("cannot xor {:?} and {:?}", self, other),
        }
    }
    pub fn shl(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x << y),
            _ => panic!("cannot shl {:?} and {:?}", self, other),
        }
    }
    pub fn shr(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Int(x >> y),
            _ => panic!("cannot shr {:?} and {:?}", self, other),
        }
    }
    pub fn eq(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(x), Self::Bool(y)) => Self::Bool(*x == *y),
            (Self::Int(x), Self::Int(y)) => Self::Bool(*x == *y),
            (Self::Float(x), Self::Float(y)) => Self::Bool(*x == *y),
            (Self::String(x), Self::String(y)) => Self::Bool(*x == *y),
            _ => panic!("cannot eq {:?} and {:?}", self, other),
        }
    }
    pub fn ne(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(x), Self::Bool(y)) => Self::Bool(*x != *y),
            (Self::Int(x), Self::Int(y)) => Self::Bool(*x != *y),
            (Self::Float(x), Self::Float(y)) => Self::Bool(*x != *y),
            (Self::String(x), Self::String(y)) => Self::Bool(*x != *y),
            _ => panic!("cannot ne {:?} and {:?}", self, other),
        }
    }
    pub fn lt(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Bool(*x < *y),
            (Self::Float(x), Self::Float(y)) => Self::Bool(*x < *y),
            (Self::String(x), Self::String(y)) => Self::Bool(*x < *y),
            _ => panic!("cannot lt {:?} and {:?}", self, other),
        }
    }
    pub fn le(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Bool(*x <= *y),
            (Self::Float(x), Self::Float(y)) => Self::Bool(*x <= *y),
            (Self::String(x), Self::String(y)) => Self::Bool(*x <= *y),
            _ => panic!("cannot le {:?} and {:?}", self, other),
        }
    }
    pub fn gt(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Bool(*x > *y),
            (Self::Float(x), Self::Float(y)) => Self::Bool(*x > *y),
            (Self::String(x), Self::String(y)) => Self::Bool(*x > *y),
            _ => panic!("cannot gt {:?} and {:?}", self, other),
        }
    }
    pub fn ge(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => Self::Bool(*x >= *y),
            (Self::Float(x), Self::Float(y)) => Self::Bool(*x >= *y),
            (Self::String(x), Self::String(y)) => Self::Bool(*x >= *y),
            _ => panic!("cannot ge {:?} and {:?}", self, other),
        }
    }
    pub fn logic_and(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(x), Self::Bool(y)) => Self::Bool(*x && *y),
            _ => panic!("cannot logic_and {:?} and {:?}", self, other),
        }
    }
    pub fn logic_or(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(x), Self::Bool(y)) => Self::Bool(*x || *y),
            _ => panic!("cannot logic_or {:?} and {:?}", self, other),
        }
    }
}


#[derive(Clone)]
pub struct NodeRef<Node> {
    inner: Rc<Node>,
    attrs: HashMap<String, String>,
}
impl<Node> NodeRef<Node> {
    pub fn new(node: Node) -> Self {
        Self {
            inner: Rc::new(node),
            attrs: HashMap::new(),
        }
    }
    pub fn as_ref(&self) -> &Node {
        &self.inner.as_ref()
    }
    #[allow(dead_code)]
    pub fn as_ptr(&self) -> *const Node {
        self.inner.as_ref() as *const Node
    }

    #[allow(dead_code)]
    pub fn set_attr(&mut self, key: String, value: String) {
        self.attrs.insert(key, value);
    }
    #[allow(dead_code)]
    pub fn get_attr(&self, key: &str) -> Option<&str> {
        self.attrs.get(key).map(|x| x.as_str())
    }
}
impl<Node> PartialEq for NodeRef<Node> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}
impl<Node> Eq for NodeRef<Node> {}
impl<Node> std::hash::Hash for NodeRef<Node> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.inner).hash(state)
    }
}
impl<Node: std::fmt::Debug> std::fmt::Debug for NodeRef<Node> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attrs = self.attrs.iter().map(|(k, v)| format!(", {}={:?}", k, v)).collect::<String>();
        f.write_str(format!("({:?}{}) ", self.inner.as_ref() as *const Node, attrs).as_str())?;
        self.inner.as_ref().fmt(f)
    }
}
impl<Node> std::ops::Deref for NodeRef<Node> {
    type Target = Node;
    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

#[macro_export]
macro_rules! def_into_node_ref {
    ($($name:ident,)+) => {
        impl std::fmt::Debug for Node {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Node::$name(x) => x.fmt(f),
                    )+
                }
            }
        }
        $(
            paste::paste! {
                impl [<Node $name>] {
                    pub fn into_node_ref(self) -> NodeRef {
                        NodeRef::new(Node::$name(self))
                    }
                }
                impl NodeRef {
                    #[allow(dead_code)]
                    pub fn [<as_ $name:lower>](&self) -> Option<&[<Node $name>]> {
                        match self.as_ref() {
                            Node::$name(x) => Some(x),
                            _ => None,
                        }
                    }
                    #[allow(dead_code)]
                    pub fn [<is_ $name:lower>](&self) -> bool {
                        self.[<as_ $name:lower>]().is_some()
                    }
                }
            }
        )+
    };
}
