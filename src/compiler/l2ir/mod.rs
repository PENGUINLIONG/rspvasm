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

#[derive(Clone)]
pub enum Node {
    Constant(NodeConstant),
    Instr(NodeInstr),
    Block(NodeBlock),
}
def_into_node_ref!(
    Constant,
    Instr,
    Block,
);

pub type NodeRef = super::common::NodeRef<Node>;

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
                            let id_token = self.force_lower(operand)?;
                            l1ir::Operand::IdRef(id_token.clone())
                        },
                    };
                    operands.push(operand);
                }
                let result_type = match &instr.result_type {
                    Some(result_type) => {
                        let result_type = self.force_lower(result_type)?;
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
        };

        if let Some(x) = out.as_ref() {
            self.node2node.insert(node_ref.clone(), x.clone());
        }

        Ok(out)
    }

    pub fn apply(node: &NodeRef) -> Result<l1ir::NodeRef> {
        let mut x = Self::new();
        let out = x.lower(&node)?
            .ok_or_else(|| anyhow!("cannot lower non-instruction nodes"))?;
        Ok(out)
    }
}
