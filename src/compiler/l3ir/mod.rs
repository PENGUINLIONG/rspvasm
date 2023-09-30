//! Level-4 Intermediate Representation
//!
//! Named lookup.
use std::collections::HashMap;
use anyhow::{anyhow, Result, bail};

use crate::def_into_node_ref;

use super::common::ConstantValue;
use super::l2ir;

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
    Variable(NodeVariable),
    Load(NodeLoad),
    Store(NodeStore),
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
    Variable,
    Load,
    Store,
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
                .ok_or_else(|| anyhow!("force lower must receive a transformed node"))
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
            Node::Variable(_) => {
                Some(self.clone())
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
            .ok_or_else(|| anyhow!("force lower must receive a transformed node"))
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

            Node::Arg(_) | Node::Constant(_) | Node::Layout(_) | Node::Instr(_) | Node::Instantiate(_) | Node::Emit(_) | Node::IfThenElse(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) => {
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
            .ok_or_else(|| anyhow!("force lower must receive a transformed node"))
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
                        Node::Instantiate(_) | Node::Emit(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) => self.lower(&node)?,
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

            Node::IfThenElse(if_then_else) => {
                let cond = self.force_lower(&if_then_else.cond)?
                    .as_constant()
                    .and_then(|x| x.value.as_bool())
                    .ok_or_else(|| anyhow!("if-then-else condition must be a bool constant"))?;

                let then_node = self.force_lower(&if_then_else.then_node)?;
                assert!(then_node.is_block(), "then node must be a block");
                let else_node = self.force_lower(&if_then_else.else_node)?;
                assert!(else_node.is_block(), "else node must be a block");
                
                let out = if cond { then_node } else { else_node };

                Some(out)
            },
            
            Node::Define(_) | Node::Lookup(_) => unreachable!(),
            Node::Constant(_) | Node::Instr(_) | Node::Emit(_) | Node::Layout(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) => {
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
                    .filter(|x| *x < u16::MAX as u32)
                    .ok_or_else(|| anyhow!("layout op must be an u16 opcode"))?;
                let position = layout.position.as_ref()
                    .as_constant()
                    .and_then(|x| x.as_float())
                    .ok_or_else(|| anyhow!("layout position must be a float"))?;

                self.layouts.insert(opcode, position);
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

    pub fn declare(&mut self, variable: &NodeRef) -> Result<()> {
        if self.variables.contains_key(&variable) {
            bail!("variable already declared: {:?}", variable);
        }
        self.variables.insert(variable.clone(), None);
        Ok(())
    }

    pub fn store(&mut self, variable: &NodeRef, value: NodeRef) -> Result<()> {
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

    pub fn eval_constexpr(&mut self, node: &NodeRef) -> Result<ConstantValue> {
        let node = self.force_lower(node)?;

        match node.as_ref() {
            Node::Constant(constant) => {
                Ok(constant.value.clone())
            }
            // TODO: (penguinliong) Constexpr logics here. Like unary, binary
            // ops.
            _ => Err(anyhow!("not a constexpr: {:?}", node)),
        }
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
            Node::Variable(_) => {
                self.declare(node)?;
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
                let cond = self.eval_constexpr(&if_then_else.cond)?;
                let then_node = self.force_lower(&if_then_else.then_node)?;
                let else_node = self.force_lower(&if_then_else.else_node)?;

                if let Some(succ) = cond.as_bool() {
                    if succ {
                        Some(then_node)
                    } else {
                        Some(else_node)
                    }
                } else {
                    bail!("if-then-else condition must be a bool constant")
                }
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

pub struct Lower {
    cache: HashMap<NodeRef, l2ir::NodeRef>,
}
impl Lower {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    pub fn force_lower(&mut self, node: &NodeRef, root_nodes: &mut Vec<l2ir::NodeRef>) -> Result<l2ir::NodeRef> {
        self.lower(node, root_nodes)?
            .ok_or_else(|| anyhow!("force lower must receive a transformed node: {:?}", node))
    }
    pub fn lower(&mut self, node: &NodeRef, root_nodes: &mut Vec<l2ir::NodeRef>) -> Result<Option<l2ir::NodeRef>> {
        if let Some(x) = self.cache.get(&node) {
            return Ok(Some(x.clone()));
        }

        let out = match node.as_ref() {
            Node::Constant(constant) => {
                let node = l2ir::NodeConstant {
                    value: constant.value.clone(),
                }.into_node_ref();
                Some(node)
            },
            Node::Block(_) => None,
            Node::Instr(_) => None,
            Node::Instantiate(instantiate) => {
                match instantiate.node.as_ref() {
                    Node::Block(block) => {
                        let mut root_nodes2 = Vec::new();
                        for node in block.nodes.iter() {
                            self.lower(node, &mut root_nodes2)?;
                        }
                        let block_node = l2ir::NodeBlock {
                            nodes: root_nodes2,
                        }.into_node_ref();
                        root_nodes.push(block_node);

                        if let Some(result_node) = block.result_node.as_ref() {
                            let node = self.cache.get(result_node)
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
                        let opcode = self.force_lower(&instr.opcode, root_nodes)?;
                        let operands = instr.operands.iter()
                            .map(|operand| self.force_lower(operand, root_nodes))
                            .collect::<Result<Vec<_>>>()?;
                        let result_type = instr.result_type.as_ref()
                            .map(|result_type| self.force_lower(result_type, root_nodes))
                            .transpose()?;

                        let node = l2ir::NodeInstr {
                            opcode,
                            operands,
                            result_type,
                            has_result: instr.has_result,
                        }.into_node_ref();

                        root_nodes.push(node.clone());
                        Some(node)
                    },
                    _ => bail!("expected instr, got: {:?}", emit.instr),
                }
            },

            Node::Define(_) | Node::Lookup(_) | Node::Arg(_) | Node::Layout(_) | Node::IfThenElse(_) | Node::Variable(_) | Node::Load(_) | Node::Store(_) => unreachable!("{:?}", node),
        };

        if let Some(out) = &out {
            self.cache.insert(node.clone(), out.clone());
        }

        Ok(out)
    }

    pub fn apply(node: &NodeRef) -> Result<(l2ir::NodeRef, HashMap<u32, f32>)> {
        let node = InlineLookups::apply(&node)?;
        let node = InstantiateBlocks::apply(&node)?;
        let (node, layouts) = CollectLayouts::apply(&node)?;
        let node = EvaluateControlFlow::apply(&node)?;

        let mut x = Self::new();
        let mut root_nodes = Vec::new();
        x.lower(&node, &mut root_nodes)?;
        Ok((root_nodes[0].clone(), layouts))
    }
}
