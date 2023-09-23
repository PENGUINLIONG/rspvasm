//! # Lower to L4 IR.
use anyhow::{anyhow, Result};

use crate::compiler::common::ConstantValue;
use crate::compiler::l3ir::*;

use super::path::Path;
use super::block::Block;
use super::stmt::Stmt;
use super::expr::Expr;
use super::pat::Pat;
use super::token::Lit;

#[derive(Debug)]
pub struct LowerToAst {
}
impl LowerToAst {
    pub fn lower_lit(&mut self, lit: &Lit) -> Result<NodeRef> {
        let node = match lit {
            Lit::Bool(value) => {
                NodeConstant {
                    value: ConstantValue::Bool(*value),
                }.into_node_ref()
            }
            Lit::Int(value) => {
                NodeConstant {
                    value: ConstantValue::Int(*value as u32),
                }.into_node_ref()
            }
            Lit::Float(value) => {
                NodeConstant {
                    value: ConstantValue::Float(*value as f32),
                }.into_node_ref()
            }
            Lit::String(value) => {
                NodeConstant {
                    value: ConstantValue::String(value.clone()),
                }.into_node_ref()
            }
        };
        Ok(node)
    }
    
    pub fn lower_path(&mut self, path: &Path) -> Result<NodeRef> {
        let mut name = String::new();
        for segment in path.segments.iter() {
            if !name.is_empty() {
                name.push_str("::");
            }
            name.push_str(&segment.name);
        }

        let node = NodeLookup {
            name,
        }.into_node_ref();
        Ok(node)
    }

    pub fn lower_pat(&mut self, pat: &Pat) -> Result<Option<NodeRef>> {
        match pat {
            Pat::Literal(literal) => {
                let node = self.lower_lit(&literal.literal.lit)?;
                Ok(Some(node))
            }
            Pat::Path(path) => {
                let node = self.lower_path(&path.path)?;
                Ok(Some(node))
            }
            Pat::Underscore(_) => {
                Ok(None)
            }
        }
    }

    pub fn lower_expr(&mut self, expr: &Expr) -> Result<NodeRef> {
        match expr {
            Expr::Literal(literal) => {
                let node = self.lower_lit(&literal.literal.lit)?;
                Ok(node)
            }
            Expr::Path(path) => {
                let node = self.lower_path(&path.path)?;
                Ok(node)
            }
            Expr::Emit(emit) => {
                let opcode = self.lower_pat(&emit.opcode)?
                    .ok_or(anyhow!("emit opcode must be a literal or a path"))?;

                let result_type = if let Some((_, pat)) = &emit.result_type {
                    self.lower_pat(pat)?
                } else {
                    None
                };

                let mut operands = Vec::new();
                if let Some(x) = &emit.operands {
                    for operand in x.inner.iter() {
                        let operand = self.lower_expr(operand)?;
                        operands.push(operand);
                    }
                }

                let node = NodeEmit {
                    instr: NodeInstr {
                        opcode,
                        result_type,
                        operands,
                        has_result: emit.result_type.is_some(),
                    }.into_node_ref(),
                }.into_node_ref();
                Ok(node)
            }
            Expr::Block(block) => {
                let mut arg_define_nodes = Vec::new();
                let mut args = Vec::new();
                if let Some(params) = block.block_header.as_ref() {
                    for param in params.punctuated.iter() {
                        let arg_node = NodeArg {
                            name: param.name.clone(),
                        }.into_node_ref();
                        args.push(arg_node.clone());

                        let define_node = NodeDefine {
                            name: param.name.clone(),
                            value: arg_node,
                        }.into_node_ref();
                        arg_define_nodes.push(define_node);
                    }
                }
                let mut block_node = self.lower_block(&block.block.inner)?;
                block_node.nodes = arg_define_nodes.into_iter()
                    .chain(block_node.nodes.into_iter())
                    .collect();
                block_node.params = args;
                Ok(block_node.into_node_ref())
            }
            Expr::Call(call) => {
                let mut args = Vec::new();
                for arg in call.args.inner.args.iter() {
                    let arg = self.lower_expr(&arg.value)?;
                    args.push(arg);
                }

                let function_node = self.lower_expr(&call.expr)?;
                let node = NodeInstantiate {
                    args,
                    node: function_node,
                }.into_node_ref();
                Ok(node)
            }
            Expr::IfThenElse(if_then_else) => {
                let condition = self.lower_expr(&if_then_else.condition)?;
                let then_node = self.lower_expr(&if_then_else.then_branch)?;
                let else_node = if let Some(else_branch) = &if_then_else.else_branch {
                    self.lower_expr(&else_branch.1)?
                } else {
                    NodeBlock {
                        params: vec![],
                        nodes: vec![],
                        result_node: None,
                    }.into_node_ref()
                };

                let node = NodeInstantiate {
                    node: NodeIfThenElse {
                        cond: condition,
                        then_node,
                        else_node,
                    }.into_node_ref(),
                    args: vec![],
                }.into_node_ref();
                Ok(node)
            }
        }
    }

    pub fn lower_stmt(&mut self, stmt: &Stmt, root_nodes: &mut Vec<NodeRef>) -> Result<Option<NodeRef>> {
        match stmt {
            Stmt::Local(local) => {
                let name = local.name.name.clone();
                let value = self.lower_expr(&local.expr)?;

                // Ensure the value is realized (if not required to manually
                // emit by StmtExpr leter).
                if !local.meta_list.contains("manual") {
                    root_nodes.push(value.clone());
                }

                // Then bind it to a local name.
                let def_node = NodeDefine {
                    name,
                    value,
                }.into_node_ref();
                root_nodes.push(def_node);
                Ok(None)
            }
            Stmt::Expr(expr) => {
                let node = self.lower_expr(&expr.expr)?;
                match node.as_ref() {
                    Node::Instantiate(_) | Node::Emit(_) | Node::Lookup(_) => {
                        root_nodes.push(node.clone());
                    }
                    _ => {}
                }
                if expr.semi_token.is_some() {
                    Ok(None)
                } else {
                    Ok(Some(node))
                }
            }
            Stmt::Const(const_) => {
                for variant in const_.variants.inner.iter() {
                    let name = if let Some(stem) = &const_.name {
                        format!("{}::{}", stem.name, variant.ident.name)
                    } else {
                        variant.ident.name.to_owned()
                    };
                    let node = self.lower_expr(&variant.expr)?;

                    // Ensure it's a const node.
                    if let Node::Constant(_) = &*node.as_ref() {
                    } else {
                        return Err(anyhow::anyhow!("enum variant must be a constant"));
                    }

                    let def_node = NodeDefine {
                        name,
                        value: node,
                    }.into_node_ref();
                    root_nodes.push(def_node);
                }
                Ok(None)
            }
            Stmt::Layout(layout) => {
                for entry in layout.entries.inner.iter() {
                    if let Some(op) = self.lower_pat(&entry.pat)? {
                        let position = self.lower_expr(&entry.expr)?;

                        let def_node = NodeLayout {
                            op,
                            position,
                        }.into_node_ref();
                        root_nodes.push(def_node);
                    } else {
                        return Err(anyhow!("layout op must not be id"));
                    }
                }
                Ok(None)
            }
        }
    }

    pub fn lower_block(&mut self, block: &Block) -> Result<NodeBlock> {
        let mut root_nodes = Vec::new();
        let mut result_node = None;
        for stmt in block.stmts.iter() {
            result_node = self.lower_stmt(stmt, &mut root_nodes)?;
        }

        let node = NodeBlock {
            params: vec![],
            nodes: root_nodes,
            result_node,
        };
        Ok(node)
    }

    pub fn apply(block: &Block) -> Result<NodeRef> {
        let mut pass = Self {};
        let block = pass.lower_block(&block)?;
        let out = NodeInstantiate {
            node: block.into_node_ref(),
            args: vec![],
        }.into_node_ref();
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use rspirv::binary::Disassemble;
    use pretty_assertions::assert_eq;

    use crate::compiler::{syn::{ParseBuffer, stmt::parse_stmts}, l3ir, l2ir, l1ir, l0ir::SpirvBinary};

    use super::LowerToAst;

    fn disassemble_spirv(spirv: &[u32]) -> String {
        let mut d = rspirv::dr::Loader::new();
        rspirv::binary::parse_words(spirv, &mut d).unwrap();
        let mut module = d.module();
        module.header = None;
        module.disassemble()
    }

    #[test]
    fn test_emit_itm_instr_by_constant() {
        let code = r#"
let void = ~19 -> _;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
    }

    #[test]
    fn test_manual_no_emit() {
        let code = r#"
#[manual]
let void = ~19 -> _;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
"#.trim());
    }

    #[test]
    fn test_manual_and_emit() {
        let code = r#"
#[manual]
let void = ~19 -> _;
void;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
    }

    #[test]
    fn test_emit_itm_instr() {
        let code = r#"
const Op {
    TypeVoid = 19,
}
let void = ~Op::TypeVoid -> _;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
    }

    #[test]
    fn test_emit_instr_chain() {
        let code = r#"
const Op {
    TypeInt = 21,
    Constant = 43,
}
let int = ~Op::TypeInt(32, 1) -> _;
let constant_1 = ~Op::Constant(1) -> int;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
"#.trim());
    }

    #[test]
    fn test_name_shadowing() {
        let code = r#"
const Op {
    TypeInt = 21,
    Constant = 43,
}
let int = ~Op::TypeInt(32, 1) -> _;
let int = ~Op::TypeInt(32, 1) -> _;
let constant_1 = ~Op::Constant(1) -> int;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpTypeInt 32 1
%3 = OpConstant  %2  1
"#.trim());
    }

    #[test]
    fn test_emit_instr_chain_with_block() {
        let code = r#"const Op {
            TypeInt = 21,
            Constant = 43,
        }

        let int = {
            ~Op::TypeInt(32, 1) -> _
        }();
        let constant_1 = {
            let constant_1 = ~Op::Constant(1) -> int;
        }();
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        dbg!(&x);
        let ctxt = l1ir::Lower::apply(x).unwrap();
        dbg!(&ctxt);
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
"#.trim());
    }

    #[test]
    fn test_emit_instr_chain_with_block_and_result() {
        let code = r#"
const Op {
    TypeInt = 21,
    Constant = 43,
}

let make_constant = |value, ty| {
    let constant_1 = ~Op::Constant(value) -> ty;
};

let int = ~Op::TypeInt(32, 1) -> _;
make_constant(value: 1, ty: int);
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
"#.trim());
    }

    #[test]
    fn test_emit_instr_chain_with_block_and_result_forwarding() {
        let code = r#"
const Op {
    TypeInt = 21,
    Constant = 43,
}

let make_int_type = {
    let int = ~Op::TypeInt(32, 1) -> _;
    int
};

let make_constant = |value, ty| {
    let constant_1 = ~Op::Constant(value) -> ty;
};

make_constant(value: 1, ty: make_int_type());
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
"#.trim());
    }

    #[test]
    fn test_emit_instr_chain_with_curying_and_result() {
        let code = r#"
const Op {
    TypeInt = 21,
    Constant = 43,
}

let make_int32_type = {
    let int = ~Op::TypeInt(32, 1) -> _;
    int
};
let make_constant = |value, ty_maker| {
    let ty = ty_maker();
    let constant_1 = ~Op::Constant(value) -> ty;
};

make_constant(value: 1, ty: make_int32_type);
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
    "#.trim());
}

    #[test]
    fn test_emit_if_then_else_true() {
        let code = r#"
const Op {
    TypeInt = 21,
    Constant = 43,
}
let int = if true {
    ~Op::TypeInt(32, 1) -> _
} else {
    ~Op::TypeInt(32, 0) -> _
};
~Op::Constant(1) -> int;
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
        "#.trim());
    }

    #[test]
    fn test_emit_if_then_else_false() {
        let code = r#"
const Op {
    TypeInt = 21,
}
if false {
    ~Op::TypeInt(32, 1) -> _
} else {
    ~Op::TypeInt(32, 0) -> _
};
"#;

        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let (x, _) = l3ir::Lower::apply(&x).unwrap();
        let x = l2ir::Lower::apply(&x).unwrap();
        let ctxt = l1ir::Lower::apply(x).unwrap();
        let spirv = SpirvBinary::from_ir(ctxt);

        let dis = disassemble_spirv(&spirv.to_words());
        assert_eq!(dis, r#"
%1 = OpTypeInt 32 0
        "#.trim());
    }

}