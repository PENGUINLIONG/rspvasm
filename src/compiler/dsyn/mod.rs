mod mat;
mod par;

use anyhow::{anyhow, bail, Result};
use std::{collections::BTreeMap, rc::Rc};

pub use mat::Match;
pub use par::{Par, ParType, ParseContext};

use super::syn::token::{Spacing, Lit};

struct Type {
    name: String,
    traits: Option<String>,
}

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
    fn new(obj: Object) -> Self {
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
    fn new() -> Self {
        Self {
            items: Vec::new(),
        }
    }

    fn push(&mut self, obj: ObjectRef) {
        self.items.push(obj);
    }
    fn get(&self, idx: usize) -> Option<&ObjectRef> {
        self.items.get(idx)
    }
}

#[derive(Debug, Clone)]
pub struct CompositeObject {
    ty: String,
    attrs: BTreeMap<String, ObjectRef>,
}
impl CompositeObject {
    fn new(ty: String) -> Self {
        Self {
            ty,
            attrs: BTreeMap::new(),
        }
    }

    fn set_attr(&mut self, name: String, obj: ObjectRef) {
        self.attrs.insert(name, obj);
    }
    fn get_attr(&self, name: &str) -> Option<&ObjectRef> {
        self.attrs.get(name)
    }
}

#[derive(Debug, Clone)]
pub struct CmdConstant {
    value: ObjectRef,
}
#[derive(Debug, Clone)]
pub struct CmdPrint {
    value: Box<Cmd>,
}
#[derive(Debug, Clone)]
pub struct CmdEmit {
    opcode: String,
    args: Vec<String>,
}
#[derive(Debug, Clone)]
pub struct CmdStore {
    name: String,
    value: Box<Cmd>,
}
#[derive(Debug, Clone)]
pub struct CmdLoad {
    name: String,
}

#[derive(Debug, Clone)]
pub enum Cmd {
    Constant(CmdConstant),
    Print(CmdPrint),
    Emit(CmdEmit),
    Store(CmdStore),
    Load(CmdLoad),
}

struct Context {
    log: Vec<String>,
    state_dict: BTreeMap<String, ObjectRef>,
}
impl Context {
    fn new() -> Self {
        Self {
            log: Vec::new(),
            state_dict: BTreeMap::new(),
        }
    }
}

struct Rule {
    par: Par,
    cmds: Vec<Cmd>,
}
impl Rule {
    fn exec_cmd_force(&self, cmd: &Cmd, ctxt: &mut Context) -> Result<ObjectRef> {
        match self.exec_cmd(cmd, ctxt)? {
            Some(obj) => Ok(obj),
            None => bail!("Command did not return a value"),
        }
    }
    fn exec_cmd(&self, cmd: &Cmd, ctxt: &mut Context) -> Result<Option<ObjectRef>> {
        let out = match cmd {
            Cmd::Constant(c) => {
                Some(c.value.clone())
            }
            Cmd::Print(c) => {
                let value = self.exec_cmd_force(&c.value, ctxt)?;
                ctxt.log.push(format!("{:?}", value));
                None
            }
            Cmd::Emit(c) => {
                unimplemented!();
            }
            Cmd::Store(c) => {
                let value = self.exec_cmd_force(&c.value, ctxt)?;
                ctxt.state_dict.insert(c.name.clone(), value);
                None
            }
            Cmd::Load(c) => {
                let value = ctxt.state_dict
                    .get(&c.name)
                    .ok_or_else(|| anyhow!("Unknown state variable {}", c.name))?;
                Some(value.clone())
            }
        };
        Ok(out)
    }

    fn mat2obj(&self, mat: &Match) -> Result<ObjectRef> {
        let obj = match mat {
            Match::None => Object::None,
            Match::Ident(ident) => {
                Object::Ident(ident.name.clone())
            },
            Match::Literal(literal) => {
                match &literal.lit {
                    Lit::Bool(x) => Object::Bool(*x),
                    Lit::Int(x) => Object::Int(*x),
                    Lit::Float(x) => Object::Float(*x),
                    Lit::String(x) => Object::String(x.clone()),
                }
            },
            Match::Punct(punct) => {
                Object::Punct(punct.ch, punct.spacing.clone())
            },
            Match::List(list) => {
                let mut arr = ArrayObject::new();
                for item in list {
                    let x = self.mat2obj(item)?;
                    arr.push(x);
                }
                Object::Array(arr)
            },
            Match::Dict(dict) => {
                let mut obj = CompositeObject::new("".to_string());
                for (name, item) in dict {
                    let x = self.mat2obj(item)?;
                    obj.set_attr(name.clone(), x);
                }
                Object::Composite(obj)
            },
        };

        let out = ObjectRef::new(obj);
        Ok(out)
    }

    pub fn convert(&self, input: &str, ctxt: &mut Context) -> Result<()> {
        let mat = self.par.parse(input)?;

        let dict = mat.as_dict()?
            .get("__")
            .ok_or_else(|| anyhow!("global scope must be named as `__`"))?
            .as_dict()?;
        for (k, v) in dict.iter() {
            let obj = self.mat2obj(v)?;
            ctxt.state_dict.insert(k.clone(), obj);
        }

        //dbg!(&ctxt.state_dict);

        for cmd in &self.cmds {
            self.exec_cmd(cmd, ctxt)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rule_conversion_simple() {
        let par = ParType::Sequence(vec![
            ParType::Ident("x".to_string()).into_named_par("command"),
        ]).into_named_par("__");
        let cmds = vec![
            Cmd::Print(CmdPrint {
                value: Box::new(Cmd::Constant(CmdConstant {
                    value: ObjectRef::new(Object::Ident("x".to_string())),
                })),
            }),
        ];
        let rule = Rule {
            par,
            cmds,
        };

        let mut ctxt = Context {
            log: Vec::new(),
            state_dict: BTreeMap::new(),
        };
        rule.convert("x", &mut ctxt).unwrap();
        assert_eq!(ctxt.log, vec!["Ident(\"x\")"]);
    }
}
