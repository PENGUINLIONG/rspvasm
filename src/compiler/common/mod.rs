use std::{rc::Rc, collections::HashMap};

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Int(u32),
    Float(f32),
    String(String),
}
impl ConstantValue {
    pub fn as_int(&self) -> Option<u32> {
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
            Self::Int(x) => vec![*x],
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
            concat_idents::concat_idents!(ty_name = Node, $name {
                impl ty_name {
                    pub fn into_node_ref(self) -> NodeRef {
                        NodeRef::new(Node::$name(self))
                    }
                }
            });
        )+
    };
}
