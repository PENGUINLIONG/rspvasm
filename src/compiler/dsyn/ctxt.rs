use anyhow::{anyhow, Result};
use std::collections::BTreeMap;

use super::ObjectRef;

#[derive(Debug, Clone)]
pub struct CmdConstant {
    pub value: ObjectRef,
}
#[derive(Debug, Clone)]
pub struct CmdPrint {
    pub value: Box<Cmd>,
}
#[derive(Debug, Clone)]
pub struct CmdEmit {
    pub opcode: String,
    pub args: Vec<String>,
}
#[derive(Debug, Clone)]
pub struct CmdStore {
    pub name: String,
    pub value: Box<Cmd>,
}
#[derive(Debug, Clone)]
pub struct CmdLoad {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Cmd {
    Constant(CmdConstant),
    Print(CmdPrint),
    Emit(CmdEmit),
    Store(CmdStore),
    Load(CmdLoad),
}

pub struct Context {
    log: Vec<String>,
    state_dict: BTreeMap<String, ObjectRef>,
}
impl Context {
    pub fn new() -> Self {
        Self {
            log: Vec::new(),
            state_dict: BTreeMap::new(),
        }
    }

    pub fn log(&mut self, msg: String) {
        self.log.push(msg);
    }

    pub fn set_state(&mut self, name: String, value: ObjectRef) {
        self.state_dict.insert(name, value);
    }
    pub fn get_state(&mut self, name: &str) -> Result<&ObjectRef> {
        self.state_dict.get(name)
            .ok_or_else(|| anyhow!("Unknown state variable {}", name))
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::dsyn::{ParType, Rule, Object};

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
        let rule = Rule::new(par, cmds);

        let mut ctxt = Context {
            log: Vec::new(),
            state_dict: BTreeMap::new(),
        };
        rule.convert("x", &mut ctxt).unwrap();
        assert_eq!(ctxt.log, vec!["Ident(\"x\")"]);
    }
}
