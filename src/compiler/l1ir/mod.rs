//! Level-1 Intermediate Representation
//!
//! SPIR-V instructions that can refer to intermediate results by direct node
//! reference. The last node in a block node is forwarded out of the block.
//! Instruction opcode, result_type and operands are restricted to the
//! corresponsing representation.
use std::collections::HashMap;
use anyhow::{bail, Result};

use crate::def_into_node_ref;

use super::l0ir;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub enum Operand {
    Constant(Vec<u32>),
    IdRef(NodeRef),
}

#[derive(Debug, Clone)]
pub struct NodeInstr {
    pub opcode: u16,
    pub operands: Vec<Operand>,
    pub result_type: Option<NodeRef>,
    pub has_result: bool,
}
#[derive(Debug, Clone)]
pub struct NodeBlock {
    pub nodes: Vec<NodeRef>,
}

#[derive(Clone)]
pub enum Node {
    Instr(NodeInstr),
    Block(NodeBlock),
}
def_into_node_ref!(
    Instr,
    Block,
);

pub type NodeRef = super::common::NodeRef<Node>;

pub struct Lower {
    instr_ctxt: l0ir::InstrContext,
    node_id_tokens: HashMap<NodeRef, l0ir::IdToken>,
}
impl Lower {
    pub fn new() -> Self {
        Self {
            instr_ctxt: l0ir::InstrContext::default(),
            node_id_tokens: HashMap::new(),
        }
    }

    pub fn alloc_node_id(&mut self, node_ref: &NodeRef) -> Result<()> {
        if self.node_id_tokens.contains_key(node_ref) {
            return Ok(());
        }

        let node = node_ref.as_ref();
        match node {
            Node::Instr(instr) => {
                if instr.has_result {
                    self.node_id_tokens.insert(node_ref.clone(), self.instr_ctxt.alloc_id());
                }
                if let Some(result_type) = instr.result_type.as_ref() {
                    self.alloc_node_id(result_type)?;
                }
                for operand in instr.operands.iter() {
                    match operand {
                        Operand::Constant(_) => {}
                        Operand::IdRef(id_ref) => {
                            self.alloc_node_id(id_ref)?;
                        }
                    }
                }
            }
            Node::Block(block) => {
                for node_ref in block.nodes.iter() {
                    self.alloc_node_id(node_ref)?;
                }
            }
        }
        Ok(())
    }

    pub fn lower_node(&mut self, node_ref: &NodeRef) -> Result<()> {
        let node = node_ref.as_ref();
        match node {
            Node::Instr(instr) => {
                let mut builder = self.instr_ctxt.build_instr(instr.opcode);

                let mut operands = Vec::new();
                for operand in instr.operands.iter() {
                    match operand {
                        Operand::Constant(constant) => {
                            operands.extend(constant.iter().cloned());
                        }
                        Operand::IdRef(id_ref) => {
                            if let Some(id_token) = self.node_id_tokens.get(id_ref) {
                                operands.push(id_token.get());
                            } else {
                                bail!("id_ref not found");
                            }
                        }
                    }
                }
                builder.set_operands(operands);

                if let Some(result_type) = instr.result_type.as_ref() {
                    if let Some(id_token) = self.node_id_tokens.get(result_type) {
                        builder.set_result_type(id_token);
                    }
                }

                if let Some(id_token) = self.node_id_tokens.get(node_ref) {
                    builder.set_result_id(id_token);
                }
                builder.build();
            }
            Node::Block(block) => {
                for node_ref in block.nodes.iter() {
                    self.lower_node(node_ref)?;
                }
            }
        }
        Ok(())
    }

    fn lower(&mut self, node_ref: &NodeRef) -> Result<()> {
        self.alloc_node_id(&node_ref)?;
        self.lower_node(&node_ref)?;
        Ok(())
    }

    pub fn apply(node_ref: NodeRef) -> Result<l0ir::InstrContext> {
        let mut x = Lower::new();
        x.lower(&node_ref)?;
        Ok(x.instr_ctxt)
    }
}
