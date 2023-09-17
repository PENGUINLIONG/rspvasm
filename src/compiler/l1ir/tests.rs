use pretty_assertions::assert_eq;

use super::l0ir::SpirvBinary;

use super::*;


#[test]
fn test_lower_instr_to_instr() {
    let root = NodeInstr {
        opcode: spirv::Op::TypeVoid as u16,
        operands: vec![],
        result_type: None,
        has_result: true,
    }.into_node_ref();

    let ctxt = Lower::apply(root).unwrap();

    let spirv = SpirvBinary::from_ir(ctxt);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_lower_block_to_instr() {
    let root = NodeBlock {
        nodes: vec![
            NodeInstr {
                opcode: spirv::Op::TypeVoid as u16,
                operands: vec![],
                result_type: None,
                has_result: true,
            }.into_node_ref(),
        ],
    }.into_node_ref();

    let ctxt = Lower::apply(root).unwrap();

    let spirv = SpirvBinary::from_ir(ctxt);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_lower_block_in_block_to_instr() {
    let root = NodeBlock {
        nodes: vec![
            NodeBlock {
                nodes: vec![
                    NodeRef::new(Node::Instr(NodeInstr {
                        opcode: spirv::Op::TypeVoid as u16,
                        operands: vec![],
                        result_type: None,
                        has_result: true,
                    })),
                ],
            }.into_node_ref(),
        ],
    }.into_node_ref();

    let ctxt = Lower::apply(root).unwrap();

    let spirv = SpirvBinary::from_ir(ctxt);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_lower_multi_instr_block_to_instrs() {
    let root = NodeBlock {
        nodes: vec![
            NodeBlock {
                nodes: vec![
                    NodeRef::new(Node::Instr(NodeInstr {
                        opcode: spirv::Op::TypeInt as u16,
                        operands: vec![
                            Operand::Constant(vec![32]),
                            Operand::Constant(vec![0]),
                        ],
                        result_type: None,
                        has_result: true,
                    })),
                    NodeRef::new(Node::Instr(NodeInstr {
                        opcode: spirv::Op::TypeVoid as u16,
                        operands: vec![],
                        result_type: None,
                        has_result: true,
                    })),
                ],
            }.into_node_ref(),
        ],
    }.into_node_ref();

    let ctxt = Lower::apply(root).unwrap();

    let spirv = SpirvBinary::from_ir(ctxt);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeInt 32 0
%2 = OpTypeVoid
"#.trim());
}

#[test]
fn test_lower_multi_instr_in_block_hierarchy_to_instrs() {
    let root = NodeBlock {
        nodes: vec![
            NodeBlock {
                nodes: vec![
                    NodeInstr {
                        opcode: spirv::Op::TypeInt as u16,
                        operands: vec![
                            Operand::Constant(vec![32]),
                            Operand::Constant(vec![0]),
                        ],
                        result_type: None,
                        has_result: true,
                    }.into_node_ref(),
                ],
            }.into_node_ref(),
            NodeInstr {
                opcode: spirv::Op::TypeVoid as u16,
                operands: vec![],
                result_type: None,
                has_result: true,
            }.into_node_ref(),
        ],
    }.into_node_ref();

    let ctxt = Lower::apply(root).unwrap();

    let spirv = SpirvBinary::from_ir(ctxt);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeInt 32 0
%2 = OpTypeVoid
"#.trim());
}

#[test]
fn test_lower_result_in_operands() {
    let int_ = NodeRef::new(Node::Instr(NodeInstr {
        opcode: spirv::Op::TypeInt as u16,
        operands: vec![
            Operand::Constant(vec![32]),
            Operand::Constant(vec![1]),
        ],
        result_type: None,
        has_result: true,
    }));
    let constant_int_1_ = NodeRef::new(Node::Instr(NodeInstr {
        opcode: spirv::Op::Constant as u16,
        operands: vec![
            Operand::Constant(vec![1]),
        ],
        result_type: Some(int_.clone()),
        has_result: true,
    }));
    let root = NodeBlock {
        nodes: vec![
            int_,
            constant_int_1_,
        ],
    }.into_node_ref();

    let ctxt = Lower::apply(root).unwrap();

    let spirv = SpirvBinary::from_ir(ctxt);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
"#.trim());
}
