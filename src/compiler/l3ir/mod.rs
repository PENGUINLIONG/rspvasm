//! Level-4 Intermediate Representation
//!
//! Named lookup.
use std::collections::HashMap;
use anyhow::{anyhow, Result, bail};

use crate::def_into_node_ref;

use super::common::ConstantValue;
use super::l0ir;

mod ty;

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
pub struct NodeArg {
    pub name: String,
}
#[derive(Debug, Clone)]
pub struct NodeBlock {
    pub params: Vec<NodeRef>,
    pub nodes: Vec<NodeRef>,
    pub result_node: Option<NodeRef>,
}
#[derive(Debug, Clone)]
pub struct NodeInstantiate {
    pub args: Vec<NodeRef>,
    pub node: NodeRef,
}
#[derive(Debug, Clone)]
pub struct NodeEmit {
    pub instr: NodeRef,
}
#[derive(Debug, Clone)]
pub struct NodeDefine {
    pub name: String,
    pub value: NodeRef,
}
#[derive(Debug, Clone)]
pub struct NodeLookup {
    pub name: String,
}
#[derive(Debug, Clone)]
pub struct NodeLayout {
    pub op: NodeRef,
    pub position: NodeRef,
}
#[derive(Debug, Clone)]
pub struct NodeIfThenElse {
    pub cond: NodeRef,
    pub then_node: NodeRef, // Block
    pub else_node: NodeRef, // Block
}
#[derive(Debug, Clone)]
pub struct NodeWhile {
    pub cond: NodeRef,
    pub body_node: NodeRef, // Block
}
#[derive(Debug, Clone)]
pub struct NodeVariable {
    pub name: String,
    pub is_mutable: bool,
    pub init_value: Option<NodeRef>,
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
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
}
#[derive(Debug, Clone)]
pub struct NodeBinary {
    pub binary_op: BinaryOp,
    pub lhs: NodeRef,
    pub rhs: NodeRef,
}

#[derive(Clone)]
pub enum Node {
    Constant(NodeConstant),
    Instr(NodeInstr),
    Arg(NodeArg),
    Block(NodeBlock),
    Instantiate(NodeInstantiate),
    Emit(NodeEmit),
    Define(NodeDefine),
    Lookup(NodeLookup),
    Layout(NodeLayout),
    IfThenElse(NodeIfThenElse),
    While(NodeWhile),
    Variable(NodeVariable),
    Load(NodeLoad),
    Store(NodeStore),
    Binary(NodeBinary),
}
def_into_node_ref!(
    Constant,
    Instr,
    Arg,
    Block,
    Instantiate,
    Emit,
    Define,
    Lookup,
    Layout,
    IfThenElse,
    While,
    Variable,
    Load,
    Store,
    Binary,
);

impl Node {
    pub fn as_constant(&self) -> Option<&ConstantValue> {
        match self {
            Self::Constant(x) => Some(&x.value),
            _ => None,
        }
    }
}

pub type NodeRef = super::common::NodeRef<Node>;
impl NodeRef {
    pub fn force_transform<T>(&self, lower: &mut T) -> Result<NodeRef>
        where T: FnMut(&NodeRef) -> Result<Option<NodeRef>>
    {
        lower(self)?
                .ok_or_else(|| {
                    anyhow!("force lower must receive a transformed node")
                })
    }
    pub fn transform<T>(&self, lower: &mut T) -> Result<Option<NodeRef>>
        where T: FnMut(&NodeRef) -> Result<Option<NodeRef>>
    {
        let out = match self.as_ref() {
            Node::Constant(_) => {
                Some(self.clone())
            },
            Node::Instr(instr) => {
                let opcode = instr.opcode.force_transform(lower)?;
                let operands = instr.operands.iter()
                    .map(|operand| operand.force_transform(lower))
                    .collect::<Result<Vec<_>>>()?;
                let result_type = instr.result_type.as_ref()
                    .map(|result_type| result_type.force_transform(lower))
                    .transpose()?;
                let node = NodeInstr {
                    opcode,
                    operands,
                    result_type,
                    has_result: instr.has_result,
                }.into_node_ref();

                Some(node)
            },
            Node::Arg(_) => {
                Some(self.clone())
            },
            Node::Block(block) => {
                let params = block.params.iter()
                    .map(|param| param.force_transform(lower))
                    .collect::<Result<Vec<_>>>()?;
                let nodes = block.nodes.iter()
                    .filter_map(|node| lower(node).transpose())
                    .collect::<Result<Vec<_>>>()?;
                let result_node = block.result_node.as_ref()
                    .map(|result_node| result_node.force_transform(lower))
                    .transpose()?;

                let node = NodeBlock {
                    params,
                    nodes,
                    result_node,
                }.into_node_ref();

                Some(node)
            },
            Node::Instantiate(instantiate) => {
                let args = instantiate.args.iter()
                    .map(|arg| arg.force_transform(lower))
                    .collect::<Result<Vec<_>>>()?;
                let node = instantiate.node.force_transform(lower)?;

                let node = NodeInstantiate {
                    args,
                    node,
                }.into_node_ref();

                Some(node)
            },
            Node::Emit(emit) => {
                let instr = emit.instr.force_transform(lower)?;

                let node = NodeEmit {
                    instr,
                }.into_node_ref();

                Some(node)
            },
            Node::Define(define) => {
                let value = define.value.force_transform(lower)?;
                let node = NodeDefine {
                    name: define.name.clone(),
                    value,
                }.into_node_ref();

                Some(node)
            },
            Node::Lookup(_) => {
                Some(self.clone())
            },
            Node::Layout(layout) => {
                let op = layout.op.force_transform(lower)?;
                let position = layout.position.force_transform(lower)?;

                let node = NodeLayout {
                    op,
                    position,
                }.into_node_ref();

                Some(node)
            },
            Node::IfThenElse(if_then_else) => {
                let cond = if_then_else.cond.force_transform(lower)?;
                let then_node = if_then_else.then_node.force_transform(lower)?;
                let else_node = if_then_else.else_node.force_transform(lower)?;

                let node = NodeIfThenElse {
                    cond,
                    then_node,
                    else_node,
                }.into_node_ref();

                Some(node)
            },
            Node::While(while_) => {
                let cond = while_.cond.force_transform(lower)?;
                let body_node = while_.cond.force_transform(lower)?;

                let node = NodeWhile {
                    cond,
                    body_node,
                }.into_node_ref();

                Some(node)
            }
            Node::Variable(variable) => {
                let init_value = variable.init_value.as_ref()
                    .map(|x| x.force_transform(lower))
                    .transpose()?;

                let node = NodeVariable {
                    name: variable.name.clone(),
                    is_mutable: variable.is_mutable,
                    init_value,
                }.into_node_ref();

                Some(node)
            },
            Node::Load(load) => {
                let variable = load.variable.force_transform(lower)?;
                assert!(variable.is_variable(), "load variable must be a variable");

                let node = NodeLoad {
                    variable,
                }.into_node_ref();

                Some(node)
            },
            Node::Store(store) => {
                let variable = store.variable.force_transform(lower)?;
                assert!(variable.is_variable(), "load variable must be a variable");
                let value = store.value.force_transform(lower)?;

                let node = NodeStore {
                    variable,
                    value,
                }.into_node_ref();

                Some(node)
            },
            Node::Binary(binary) => {
                let lhs = binary.lhs.force_transform(lower)?;
                let rhs = binary.rhs.force_transform(lower)?;

                let node = NodeBinary {
                    binary_op: binary.binary_op.clone(),
                    lhs,
                    rhs,
                }.into_node_ref();

                Some(node)
            },
        };

        Ok(out)
    }
}

pub struct InlineLookups {
    cache: HashMap<NodeRef, NodeRef>,
    stack: Vec<HashMap<String, NodeRef>>,
}
impl InlineLookups {
    fn new() -> Self {
        Self {
            cache: HashMap::new(),
            stack: vec![],
        }
    }

    fn push(&mut self) {
        self.stack.push(HashMap::new());
    }
    fn pop(&mut self) -> HashMap<String, NodeRef> {
        self.stack.pop().unwrap()
    }

    fn define(&mut self, name: String, value: NodeRef) {
        self.stack.last_mut()
            .unwrap()
            .insert(name, value);
    }
    fn lookup(&mut self, name: &str) -> Option<NodeRef> {
        for frame in self.stack.iter_mut().rev() {
            if let Some(x) = frame.get(name) {
                return Some(x.clone());
            }
        }
        None
    }

    fn apply(root: &NodeRef) -> Result<NodeRef> {
        let mut x = Self::new();
        x.push();
        let root = x.force_lower(root)?;
        x.pop();
        Ok(root)
    }

    fn force_lower(&mut self, node: &NodeRef) -> Result<NodeRef> {
        self.lower(node)?
            .ok_or_else(|| {
                anyhow!("force lower must receive a transformed node")
            })
    }
    fn lower(&mut self, node: &NodeRef) -> Result<Option<NodeRef>> {
        if let Some(x) = self.cache.get(&node) {
            return Ok(Some(x.clone()));
        }

        let out = match node.as_ref() {
            Node::Define(define) => {
                let value = self.force_lower(&define.value)?;
                self.define(define.name.clone(), value);
                None
            },
            Node::Lookup(lookup) => {
                let node = self.lookup(&lookup.name)
                    .ok_or_else(|| anyhow!("cannot find symbol: {}", lookup.name))?;
                Some(node)
            },

            Node::Block(block) => {
                self.push();

                let params = block.params.iter()
                    .map(|param| self.force_lower(param))
                    .collect::<Result<Vec<_>>>()?;
                let nodes = block.nodes.iter()
                    .filter_map(|node| self.lower(node).transpose())
                    .collect::<Result<Vec<_>>>()?;
                let result_node = block.result_node.as_ref()
                    .map(|result_node| self.force_lower(result_node))
                    .transpose()?;

                self.pop();

                let node = NodeBlock {
                    params,
                    nodes,
                    result_node,
                }.into_node_ref();

                Some(node)
            },

            Node::Arg(_) | Node::Constant(_) | Node::Layout(_) | Node::Instr(_) | Node::Instantiate(_) | Node::Emit(_) | Node::IfThenElse(_) | Node::While(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) | Node::Binary(_) => {
                node.transform(&mut |x| {
                    self.lower(x)
                })?
            },
        };

        if let Some(out) = out.as_ref() {
            self.cache.insert(node.clone(), out.clone());
        }

        Ok(out)
    }
}

pub struct CollectLayouts {
    layouts: HashMap<u32, f32>,
}
impl CollectLayouts {
    pub fn new() -> Self {
        Self {
            layouts: HashMap::new(),
        }
    }

    pub fn lower_impl(&mut self, node: &NodeRef) -> Result<Option<NodeRef>> {
        let out = match node.as_ref() {
            Node::Layout(layout) => {
                let opcode = layout.op.as_ref()
                    .as_constant()
                    .and_then(|x| x.as_int())
                    .filter(|x| *x < u16::MAX as i32)
                    .ok_or_else(|| anyhow!("layout op must be an u16 opcode"))?;
                let position = layout.position.as_ref()
                    .as_constant()
                    .and_then(|x| x.as_float())
                    .ok_or_else(|| anyhow!("layout position must be a float"))?;

                self.layouts.insert(opcode as u32, position);
                None
            },
            _ => Some(node.clone()),
        };

        Ok(out)
    }

    pub fn lower(&mut self, node: &NodeRef) -> Result<NodeRef> {
        // We only collect layouts in the outer most block.
        let mut nodes = Vec::new();

        match node.as_ref() {
            Node::Instantiate(x) => {
                match x.node.as_ref() {
                    Node::Block(block) => {
                        for node in block.nodes.iter() {
                            if let Some(node) = self.lower_impl(node)? {
                                nodes.push(node);
                            }
                        }
                    },
                    _ => bail!("expected block"),
                }
            },
            _ => bail!("expected instantiate"),
        };

        let out_node = NodeInstantiate {
            args: vec![],
            node: NodeBlock {
                nodes,
                params: vec![],
                result_node: None,
            }.into_node_ref(),
        }.into_node_ref();

        Ok(out_node)
    }

    pub fn apply(root: &NodeRef) -> Result<(NodeRef, HashMap<u32, f32>)> {
        let mut x = Self::new();
        let out = x.lower(root)?;
        Ok((out, x.layouts))
    }
}

pub struct EvaluateControlFlow {
    variables: HashMap<NodeRef, Option<NodeRef>>,
    cache: HashMap<NodeRef, NodeRef>,
}
impl EvaluateControlFlow {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            cache: HashMap::new(),
        }
    }

    pub fn declare(&mut self, variable: &NodeRef, init_value: Option<NodeRef>) -> Result<()> {
        if self.variables.contains_key(&variable) {
            bail!("variable already declared: {:?}", variable);
        }
        self.variables.insert(variable.clone(), init_value);
        Ok(())
    }

    pub fn store(&mut self, variable: &NodeRef, value: NodeRef) -> Result<()> {
        if let Some(variable) = variable.as_variable() {
            if !variable.is_mutable {
                bail!("cannot store to immutable variable: {:?}", variable);
            }
        } else {
            bail!("cannot store to non-variable: {:?}", variable);
        }

        if let Some(constant) = self.variables.get_mut(&variable) {
            let _ = constant.insert(value);
            Ok(())
        } else {
            Err(anyhow!("undefined variable: {:?}", variable))
        }
    }

    pub fn load(&mut self, variable: &NodeRef) -> Result<NodeRef> {
        let node = self.variables.get(&variable)
            .ok_or_else(|| anyhow!("undefined variable: {:?}", variable))?
            .clone()
            .ok_or_else(|| anyhow!("variable is not assigned yet: {:?}", variable))?;
        Ok(node)
    }

    pub fn force_lower(&mut self, node: &NodeRef) -> Result<NodeRef> {
        let out = self.lower(node)?
            .ok_or_else(|| anyhow!("cannot lower non-instruction nodes"))?;
        Ok(out)
    }
    pub fn lower(&mut self, node: &NodeRef) -> Result<Option<NodeRef>> {
        if let Some(x) = self.cache.get(node) {
            return Ok(Some(x.clone()));
        }

        let out = match node.as_ref() {
            Node::Variable(variable) => {
                let init_value = variable.init_value.as_ref()
                    .map(|x| self.force_lower(x))
                    .transpose()?;
                self.declare(node, init_value)?;
                None
            },
            Node::Load(load) => {
                let node = self.load(&load.variable)?;
                Some(node)
            },
            Node::Store(store) => {
                let value = self.force_lower(&store.value)?;
                self.store(&store.variable, value)?;
                None
            },

            Node::IfThenElse(if_then_else) => {
                let cond = self.force_lower(&if_then_else.cond)?
                    .as_constant()
                    .and_then(|x| x.value.as_bool())
                    .ok_or_else(|| anyhow!("if-then-else condition must be a bool constant"))?;

                if cond {
                    let then_node = self.force_lower(&if_then_else.then_node)?;
                    assert!(then_node.is_instantiate(), "then node must be an instantiated block");
                    Some(then_node)
                } else {
                    let else_node = self.force_lower(&if_then_else.else_node)?;
                    assert!(else_node.is_instantiate(), "else node must be an instantiated block");
                    Some(else_node)
                }
            },
            Node::While(while_) => {
                let cond = self.force_lower(&while_.cond)?
                    .as_constant()
                    .and_then(|x| x.value.as_bool())
                    .ok_or_else(|| anyhow!("while condition must be a bool constant"))?;

                loop {
                    if cond {
                        let body_node = self.force_lower(&while_.body_node)?;
                        assert!(body_node.is_instantiate(), "body node must be an instantiated block");
                    } else {
                        break;
                    }
                }

                None
            }

            Node::Binary(binary) => {
                let lhs = self.force_lower(&binary.lhs)?
                    .as_constant()
                    .ok_or_else(|| anyhow!("lhs must be a constant"))?
                    .value
                    .clone();
                let rhs = self.force_lower(&binary.rhs)?
                    .as_constant()
                    .ok_or_else(|| anyhow!("rhs must be a constant"))?
                    .value
                    .clone();

                let out = match binary.binary_op {
                    BinaryOp::Add => lhs.add(&rhs),
                    BinaryOp::Sub => lhs.sub(&rhs),
                    BinaryOp::Mul => lhs.mul(&rhs),
                    BinaryOp::Div => lhs.div(&rhs),
                    BinaryOp::Rem => lhs.rem(&rhs),
                    BinaryOp::Eq => lhs.eq(&rhs),
                    BinaryOp::Ne => lhs.ne(&rhs),
                    BinaryOp::Lt => lhs.lt(&rhs),
                    BinaryOp::Le => lhs.le(&rhs),
                    BinaryOp::Gt => lhs.gt(&rhs),
                    BinaryOp::Ge => lhs.ge(&rhs),
                    BinaryOp::LogicalAnd => lhs.logic_and(&rhs),
                    BinaryOp::LogicalOr => lhs.logic_or(&rhs),
                };

                let node = NodeConstant {
                    value: out,
                }.into_node_ref();

                Some(node)
            },

            Node::Define(_) | Node::Lookup(_) | Node::Layout(_) => unreachable!(),
            Node::Arg(_) | Node::Constant(_) | Node::Instr(_) | Node::Block(_) | Node::Instantiate(_) | Node::Emit(_) => {
                node.transform(&mut |x| {
                    self.lower(x)
                })?
            },
        };

        if let Some(x) = out.as_ref() {
            self.cache.insert(node.clone(), x.clone());
        }

        Ok(out)
    }

    pub fn apply(node: &NodeRef) -> Result<NodeRef> {
        let mut x = Self::new();
        let out = x.force_lower(node)?;
        Ok(out)
    }

}

#[derive(Debug, Clone, Default)]
pub struct StackFrame {
    params: Option<Vec<NodeRef>>,
    args: Vec<NodeRef>,
}
#[derive(Debug, Clone)]
pub struct InstantiateBlocks {
    cache: HashMap<NodeRef, NodeRef>,
    stack: Vec<StackFrame>,
}
impl InstantiateBlocks {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            stack: vec![],
        }
    }

    // A stack is completely pushed first by an instantiate and then by a block.
    pub fn push_by_instantiate(&mut self, args: Vec<NodeRef>) {
        let frame = StackFrame {
            params: None,
            args,
        };
        self.stack.push(frame);
    }
    pub fn push_by_block(&mut self, params: Vec<NodeRef>) -> bool {
        let frame = self.stack.last_mut().unwrap();
        if frame.params.is_some() {
            return false;
        }

        frame.params = Some(params);
        return true;
    }
    pub fn pop_by_block(&mut self) -> Vec<NodeRef> {
        let frame = self.stack.last_mut().unwrap();
        assert!(frame.params.is_some());
        frame.params.take().unwrap()
    }
    pub fn pop_by_instantiate(&mut self) -> Vec<NodeRef> {
        assert!(self.stack.last().unwrap().params.is_none());
        self.stack.pop().unwrap().args
    }

    pub fn lookup(&mut self, param: &NodeRef) -> Option<NodeRef> {
        for frame in self.stack.iter().rev() {
            if let Some(params) = frame.params.as_ref() {
                let arg = params.iter()
                    .zip(frame.args.iter())
                    .find_map(|(xparam, xarg)| {
                        if xparam == param {
                            Some(xarg)
                        } else {
                            None
                        }
                    });
                if arg.is_some() {
                    return arg.cloned();
                }
            }
        }
        None
    }

    pub fn apply(root: &NodeRef) -> Result<NodeRef> {
        let mut x = Self::new();
        x.push_by_instantiate(vec![]);
        x.push_by_block(vec![]);
        let root = x.force_lower(root)?;
        x.pop_by_block();
        x.pop_by_instantiate();
        Ok(root)
    }

    pub fn force_lower(&mut self, node: &NodeRef) -> Result<NodeRef> {
        self.lower(node)?
            .ok_or_else(|| {
                anyhow!("force lower must receive a transformed node")
            })
    }
    pub fn lower(&mut self, node: &NodeRef) -> Result<Option<NodeRef>> {
        if let Some(x) = self.cache.get(&node) {
            return Ok(Some(x.clone()));
        }

        let out = match node.as_ref() {
            Node::Arg(arg) => {
                let node = self.lookup(node)
                    .ok_or_else(|| anyhow!("arg is not assigned: {}", arg.name))?;
                let node = self.force_lower(&node)?;
                Some(node)
            },
            Node::Block(block) => {
                self.push_by_block(block.params.clone());

                let mut nodes = Vec::new();
                for node in block.nodes.iter() {
                    let node = match node.as_ref() {
                        Node::Instantiate(_) | Node::Emit(_) | Node::IfThenElse(_) | Node::While(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) => self.lower(&node)?,
                        _ => None,
                    };
                    if let Some(node) = node {
                        nodes.push(node);
                    }
                }
                let result_node = block.result_node.as_ref()
                    .map(|result_node| self.force_lower(result_node))
                    .transpose()?;

                self.pop_by_block();
    
                let node = NodeBlock {
                    params: Vec::new(),
                    nodes,
                    result_node,
                }.into_node_ref();

                Some(node)
            },
            Node::Instantiate(instantiate) => {
                self.push_by_instantiate(instantiate.args.clone());
                let node = self.force_lower(&instantiate.node)?;
                self.pop_by_instantiate();

                match node.as_ref() {
                    Node::Block(_) => {},
                    Node::Instantiate(_) => {
                        bail!("attempting to instantiate a block that is already instantiated: {:?}", node)
                    }
                    _ => bail!("expected block, got: {:?}", instantiate.node),
                }

                let node = NodeInstantiate {
                    args: Vec::new(),
                    node,
                }.into_node_ref();

                Some(node)
            },

            Node::Define(_) | Node::Lookup(_) => unreachable!(),
            Node::Constant(_) | Node::Instr(_) | Node::Emit(_) | Node::Layout(_) | Node::IfThenElse(_) | Node::While(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) | Node::Binary(_) => {
                node.transform(&mut |x| {
                    self.lower(x)
                })?
            },
        };

        if let Some(out) = out.as_ref() {
            self.cache.insert(node.clone(), out.clone());
        }

        Ok(out)
    }
}

pub struct FlattenBlocks {
    cache: HashMap<NodeRef, NodeRef>,
    root_nodes: Vec<NodeRef>,
}
impl FlattenBlocks {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            root_nodes: Vec::new(),
        }
    }

    fn force_lower(&mut self, node: &NodeRef) -> Result<NodeRef> {
        self.lower(node)?
            .ok_or_else(|| {
                anyhow!("force lower must receive a transformed node: {:?}", node)
            })
    }
    fn lower(&mut self, node: &NodeRef) -> Result<Option<NodeRef>> {
        if let Some(x) = self.cache.get(&node) {
            return Ok(Some(x.clone()));
        }

        let out = match node.as_ref() {
            Node::Constant(_) => {
                Some(node.clone())
            },
            Node::Emit(emit) => {
                if let Some(instr) = emit.instr.as_instr() {
                    let opcode = self.force_lower(&instr.opcode)?;
                    let operands = instr.operands.iter()
                        .map(|operand| self.force_lower(operand))
                        .collect::<Result<Vec<_>>>()?;
                    let result_type = instr.result_type.as_ref()
                        .map(|result_type| self.force_lower(result_type))
                        .transpose()?;

                    let node = NodeEmit {
                        instr: NodeInstr {
                            opcode,
                            operands,
                            result_type,
                            has_result: instr.has_result,
                        }.into_node_ref(),
                    }.into_node_ref();

                    self.root_nodes.push(node.clone());

                    Some(node)
                } else {
                    bail!("expected instr, got: {:?}", emit.instr);
                }
            },
            Node::Instantiate(instantiate) => {
                if let Some(block) = instantiate.node.as_block() {
                    for node in block.nodes.iter() {
                        self.lower(node)?;
                    }
    
                    let result_node = block.result_node.as_ref()
                        .map(|result_node| self.force_lower(result_node))
                        .transpose()?;
    
                    result_node
                } else {
                    bail!("expected block, got: {:?}", instantiate.node);
                }
            },
            _ => None,
        };

        if let Some(out) = out.as_ref() {
            self.cache.insert(node.clone(), out.clone());
        }

        Ok(out)
    }

    pub fn apply(node: &NodeRef) -> Result<NodeRef> {
        let mut x = Self::new();
        x.lower(node)?;
        let out = NodeInstantiate {
            node: NodeBlock {
                nodes: x.root_nodes,
                params: vec![],
                result_node: None,
            }.into_node_ref(),
            args: vec![],
        }.into_node_ref();
        Ok(out)
    }
}

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

    pub fn alloc_node_id(&mut self, node: &NodeRef) -> Result<()> {
        if self.node_id_tokens.contains_key(node) {
            return Ok(());
        }

        match node.as_ref() {
            Node::Constant(_) | Node::Block(_) | Node::Instr(_) => {},
            Node::Emit(emit) => {
                match emit.instr.as_ref() {
                    Node::Instr(instr) => {
                        self.alloc_node_id(&instr.opcode)?;
                        for operand in instr.operands.iter() {
                            self.alloc_node_id(operand)?;
                        }
                        if let Some(result_type) = instr.result_type.as_ref() {
                            self.alloc_node_id(result_type)?;
                        }

                        if instr.has_result {
                            self.node_id_tokens.insert(node.clone(), self.instr_ctxt.alloc_id());
                        }
                    },
                    _ => unreachable!(),
                }
            },
            Node::Instantiate(instantiate) => {
                match instantiate.node.as_ref() {
                    Node::Block(block) => {
                        for node in block.nodes.iter() {
                            self.alloc_node_id(node)?;
                        }
                    }
                    _ => unreachable!(),
                }
            },

            Node::Define(_) | Node::Lookup(_) | Node::Arg(_) | Node::Layout(_) | Node::IfThenElse(_) | Node::While(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) | Node::Binary(_) => unreachable!("{:?}", node),
        }

        Ok(())
    }

    pub fn lower(&mut self, node: &NodeRef) -> Result<Option<l0ir::IdToken>> {
        let out = match node.as_ref() {
            Node::Constant(_) => None,
            Node::Block(_) => None,
            Node::Instr(_) => None,
            Node::Instantiate(instantiate) => {
                // There shall be only a single layer of block.
                match instantiate.node.as_ref() {
                    Node::Block(block) => {
                        for node in block.nodes.iter() {
                            self.lower(node)?;
                        }

                        if let Some(result_node) = block.result_node.as_ref() {
                            let node = self.node_id_tokens.get(result_node)
                                .ok_or_else(|| anyhow!("result node not found"))?;
                            Some(node.clone())
                        } else {
                            None
                        }
                    },
                    _ => bail!("expected block, got: {:?}", instantiate.node),
                }
            },
            Node::Emit(emit) => {
                match emit.instr.as_ref() {
                    Node::Instr(instr) => {
                        let opcode = instr.opcode.as_constant()
                            .and_then(|x| x.value.as_int())
                            .and_then(|x| {
                                if x < u16::MAX as i32 {
                                    Some(x as u16)
                                } else {
                                    None
                                }
                            })
                            .ok_or_else(|| anyhow!("opcode must be an u16"))?;
                        let mut builder = self.instr_ctxt.build_instr(opcode);

                        let mut operands = Vec::new();
                        for operand in instr.operands.iter() {
                            if let Some(x) = operand.as_constant() {
                                operands.extend(x.value.to_words())
                            } else {
                                if let Some(id_token) = self.node_id_tokens.get(operand) {
                                    operands.push(id_token.get());
                                } else {
                                    bail!("id_ref not found: {:?}", operand);
                                }
                            };
                        }
                        builder.set_operands(operands);
        
                        if let Some(result_type) = instr.result_type.as_ref() {
                            if let Some(id_token) = self.node_id_tokens.get(result_type) {
                                builder.set_result_type(id_token);
                            }
                        }

                        if let Some(id_token) = self.node_id_tokens.get(node) {
                            builder.set_result_id(id_token);
                        }
                        builder.build()
                    },
                    _ => bail!("expected instr, got: {:?}", emit.instr),
                }
            },

            Node::Define(_) | Node::Lookup(_) | Node::Arg(_) | Node::Layout(_) | Node::IfThenElse(_) | Node::While(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) | Node::Binary(_) => unreachable!("{:?}", node),
        };

        Ok(out)
    }

    pub fn apply(node: &NodeRef) -> Result<l0ir::InstrContext> {
        let node = InlineLookups::apply(&node)?;
        let (node, layouts) = CollectLayouts::apply(&node)?;
        let node = EvaluateControlFlow::apply(&node)?;
        let node = InstantiateBlocks::apply(&node)?;
        dbg!(&node);
        let node = FlattenBlocks::apply(&node)?;

        let mut x = Self::new();
        x.alloc_node_id(&node)?;
        x.lower(&node)?;
        x.instr_ctxt.sort_by_layouts(layouts);
        dbg!(&x.instr_ctxt);
        Ok(x.instr_ctxt)
    }
}
