use anyhow::{anyhow, bail, Result};
use crate::compiler::syn::token::Lit;

use super::{Par, Cmd, Context, ObjectRef, Object, Match, ArrayObject, CompositeObject};



pub struct Rule {
    par: Par,
    cmds: Vec<Cmd>,
}
impl Rule {
    pub fn new(par: Par, cmds: Vec<Cmd>) -> Self {
        Self {
            par,
            cmds,
        }
    }

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
                ctxt.log(format!("{:?}", value));
                None
            }
            Cmd::Emit(c) => {
                unimplemented!();
            }
            Cmd::Store(c) => {
                let value = self.exec_cmd_force(&c.value, ctxt)?;
                ctxt.set_state(c.name.clone(), value);
                None
            }
            Cmd::Load(c) => {
                let value = ctxt.get_state(&c.name)?;
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
            ctxt.set_state(k.clone(), obj);
        }

        //dbg!(&ctxt.state_dict);

        for cmd in &self.cmds {
            self.exec_cmd(cmd, ctxt)?;
        }
        Ok(())
    }
}
