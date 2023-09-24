//! Level-2 Intermediate Representation
//!
//! Abstract node graph of SPIR-V program.
use std::{collections::HashMap, os::fd::AsRawFd, any};
use anyhow::{anyhow, bail, Result};
use rspirv::sr::Constant;

use crate::def_into_node_ref;

use super::{l1ir, common::ConstantValue};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct NodeConstant {
    pub value: ConstantValue,
}
#[derive(Debug, Clone)]
pub struct NodeInstr {
    pub opcode: NodeRef,
    pub operands: Vec<NodeRef>,
    pub result_type: Option<NodeRef>,
    pub has_result: bool,
}
#[derive(Debug, Clone)]
pub struct NodeBlock {
    pub nodes: Vec<NodeRef>,
}
#[derive(Debug, Clone)]
pub struct NodeVariable {
    pub name: String,
}
#[derive(Debug, Clone)]
pub struct NodeLoad {
    pub variable: NodeRef,
}
#[derive(Debug, Clone)]
pub struct NodeStore {
    pub variable: NodeRef,
    pub value: NodeRef,
}
#[derive(Debug, Clone)]
pub struct NodeJump {
    pub cond: Option<NodeRef>,
    pub offset: isize,
}

#[derive(Clone)]
pub enum Node {
    Constant(NodeConstant),
    Instr(NodeInstr),
    Block(NodeBlock),
    Variable(NodeVariable),
    Load(NodeLoad),
    Store(NodeStore),
    Jump(NodeJump),
}
def_into_node_ref!(
    Constant,
    Instr,
    Block,
    Variable,
    Load,
    Store,
    Jump,
);

pub type NodeRef = super::common::NodeRef<Node>;

pub struct EvaluateControlFlow {
    variables: HashMap<NodeRef, Option<ConstantValue>>,
}
impl EvaluateControlFlow {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn store_constant(&mut self, variable: &NodeRef, value: ConstantValue) -> Result<()> {
        if let Some(constant) = self.variables.get_mut(&variable) {
            constant.insert(value);
            Ok(())
        } else {
            Err(anyhow!("undefined variable: {:?}", variable))
        }
    }

    pub fn load_constant(&mut self, variable: &NodeRef) -> Result<ConstantValue> {
        let constant = self.variables.get(&variable)
            .ok_or_else(|| anyhow!("undefined variable: {:?}", variable))?
            .clone()
            .ok_or_else(|| anyhow!("variable is not assigned yet: {:?}", variable))?;
        Ok(constant)
    }

    pub fn eval_constexpr(&mut self, node: &NodeRef) -> Result<ConstantValue> {
        match node.as_ref() {
            Node::Constant(constant) => {
                Ok(constant.value.clone())
            }
            Node::Load(load) => {
                let constant = self.load_constant(&load.variable)?;
                Ok(constant.clone())
            }
            // TODO: (penguinliong) Constexpr logics here. Like unary, binary
            // ops.
            _ => Err(anyhow!("not a constexpr: {:?}", node)),
        }
    }

    pub fn lower_block(&mut self, block: &NodeBlock) -> Result<NodeBlock> {
        let mut i = block.nodes.len();
        let mut out_nodes = vec![];
        while i < block.nodes.len() {
            if let Some(node) = block.nodes.get(i) {
                let out = match node.as_ref() {
                    Node::Constant(_) => None,
                    Node::Instr(_) => {
                        Some(node.clone())
                    },
                    Node::Block(block) => {
                        let block = self.lower_block(block)?;
                        Some(block.into_node_ref())
                    },
                    Node::Variable(_) => {
                        self.variables.insert(node.clone(), None);
                        None
                    },
                    Node::Load(load) => {
                        let constant = self.load_constant(&load.variable)?;
                        let node = NodeConstant {
                            value: constant,
                        }.into_node_ref();
                        Some(node)
                    },
                    Node::Store(store) => {
                        let value = self.eval_constexpr(&store.value)?;
                        if let Some(constant) = self.variables.get_mut(&store.variable) {
                            let _ = constant.insert(value);
                        } else {
                            return Err(anyhow!("undefined "))
                        }
                        None
                    },
                    Node::Jump(jump) => {
                        let succ = if let Some(cond) = &jump.cond {
                            if let ConstantValue::Bool(value) = self.eval_constexpr(&cond)? {
                                value
                            } else {
                                bail!("jump condition must be a bool constant")
                            }
                        } else {
                            true
                        };

                        if succ {
                            if jump.offset < 0 {
                                i -= (-jump.offset) as usize;
                            } else {
                                i += jump.offset as usize;
                            }
                            continue;
                        }
                        None
                    },
                };

                if let Some(node) = out {
                    out_nodes.push(node);
                }

                i += 1;
            } else {
                break;
            }
        }

        let out = NodeBlock {
            nodes: out_nodes,
        };
        Ok(out)
    }

}

pub struct Lower {
    node2node: HashMap<NodeRef, l1ir::NodeRef>,
}
impl Lower {
    pub fn new() -> Self {
        Self {
            node2node: HashMap::new(),
        }
    }

    pub fn get_constant(&self, node_ref: &NodeRef) -> Option<ConstantValue> {
        match node_ref.as_ref() {
            Node::Constant(constant) => Some(constant.value.clone()),
            _ => None,
        }
    }

    pub fn force_lower(&mut self, node_ref: &NodeRef) -> Result<l1ir::NodeRef> {
        let out = self.lower(node_ref)?
            .ok_or_else(|| anyhow!("cannot lower non-instruction nodes"))?;
        Ok(out)
    }
    pub fn lower(&mut self, node_ref: &NodeRef) -> Result<Option<l1ir::NodeRef>> {
        if let Some(x) = self.node2node.get(node_ref) {
            return Ok(Some(x.clone()));
        }

        let out = match node_ref.as_ref() {
            Node::Constant(_) => {
                None
            },
            Node::Instr(instr) => {
                let opcode = match self.get_constant(&instr.opcode) {
                    Some(ConstantValue::Int(x)) if x <= u16::MAX as u32 => {
                        x as u16
                    },
                    _ => bail!("expected int16 opcode"),
                };
                let mut operands = Vec::new();
                for operand in &instr.operands {
                    let operand = match self.get_constant(operand) {
                        Some(constant) => {
                            let constant = constant.to_words();
                            l1ir::Operand::Constant(constant)
                        },
                        None => {
                            let id_token = self.lower(operand)?
                                .ok_or_else(|| anyhow!("operand should have been lowered but actually wasn't"))?;
                            l1ir::Operand::IdRef(id_token.clone())
                        },
                    };
                    operands.push(operand);
                }
                let result_type = match &instr.result_type {
                    Some(result_type) => {
                        let result_type = self.lower(result_type)?
                            .ok_or_else(|| anyhow!("result type should have been lowered but actually wasn't"))?;
                        Some(result_type.clone())
                    },
                    None => None,
                };
                let has_result = instr.has_result;

                let instr = l1ir::NodeInstr {
                    opcode,
                    operands,
                    result_type,
                    has_result,
                }.into_node_ref();
                self.node2node.insert(node_ref.clone(), instr.clone());

                Some(instr)
            },
            Node::Block(block) => {
                let mut nodes = Vec::new();
                for node in &block.nodes {
                    if let Some(node) = self.lower(node)? {
                        nodes.push(node);
                    }
                }
                let block = l1ir::NodeBlock {
                    nodes,
                }.into_node_ref();
                Some(block)
            },

            Node::Variable(_) | Node::Load(_) | Node::Store(_) | Node::Jump(_) => unreachable!(),
        };

        if let Some(x) = out.as_ref() {
            self.node2node.insert(node_ref.clone(), x.clone());
        }

        Ok(out)
    }

    pub fn apply(node_ref: &NodeRef) -> Result<l1ir::NodeRef> {
        let mut x = Self::new();
        let out = x.lower(node_ref)?
            .ok_or_else(|| anyhow!("cannot lower non-instruction nodes"))?;
        Ok(out)
    }
}
