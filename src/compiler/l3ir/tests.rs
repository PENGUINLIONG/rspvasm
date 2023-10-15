use super::*;

use rspirv::binary::Disassemble;
use crate::compiler::{l3ir, l0ir::SpirvBinary};

use pretty_assertions::assert_eq;


fn disassemble_spirv(spirv: &[u32]) -> String {
    let mut d = rspirv::dr::Loader::new();
    rspirv::binary::parse_words(spirv, &mut d).unwrap();
    let mut module = d.module();
    module.header = None;
    module.disassemble()
}

fn make_int_constant(x: i32) -> NodeRef {
    NodeConstant {
        value: ConstantValue::Int(x),
    }.into_node_ref()
}
fn make_op_constant(opcode: spirv::Op) -> NodeRef {
    make_int_constant(opcode as i32)
}

#[test]
fn test_simple() {
    let root = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                NodeEmit {
                    instr: NodeInstr {
                        opcode: make_op_constant(spirv::Op::TypeVoid),
                        operands: vec![],
                        result_type: None,
                        has_result: true,
                    }.into_node_ref(),
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&root).unwrap();
    let spirv = SpirvBinary::from_ir(x);

    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_define_instr() {
    let root = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                NodeDefine {
                    name: "foo".to_string(),
                    value: NodeEmit {
                        instr: NodeInstr {
                            opcode: make_op_constant(spirv::Op::TypeVoid),
                            operands: vec![],
                            result_type: None,
                            has_result: true,
                        }.into_node_ref(),
                    }.into_node_ref(),
                }.into_node_ref(),
                NodeLookup {
                    name: "foo".to_string(),
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&root).unwrap();
    let spirv = SpirvBinary::from_ir(x);

    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_define_block_but_not_instantiate() {
    let root = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                NodeDefine {
                    name: "foo".to_string(),
                    value: NodeBlock {
                        nodes: vec![
                            NodeEmit {
                                instr: NodeInstr {
                                    opcode: make_op_constant(spirv::Op::TypeVoid),
                                    operands: vec![],
                                    result_type: None,
                                    has_result: true,
                                }.into_node_ref(),
                            }.into_node_ref(),
                        ],
                        params: vec![],
                        result_node: None,
                    }.into_node_ref(),
                }.into_node_ref(),
                NodeLookup {
                    name: "foo".to_string(),
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&root).unwrap();
    let spirv = SpirvBinary::from_ir(x);

    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(dis, r#"
"#.trim());
}

#[test]
fn test_define_block() {
    let root = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                NodeDefine {
                    name: "foo".to_string(),
                    value: NodeBlock {
                        nodes: vec![
                            NodeEmit {
                                instr: NodeInstr {
                                    opcode: make_op_constant(spirv::Op::TypeVoid),
                                    operands: vec![],
                                    result_type: None,
                                    has_result: true,
                                }.into_node_ref(),
                            }.into_node_ref(),
                        ],
                        params: vec![],
                        result_node: None,
                    }.into_node_ref(),
                }.into_node_ref(),
                NodeInstantiate {
                    node: NodeLookup {
                        name: "foo".to_string(),
                    }.into_node_ref(),
                    args: vec![],
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&root).unwrap();
    let spirv = SpirvBinary::from_ir(x);

    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_cross_block_lookup() {
    let root = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                NodeDefine {
                    name: "foo".to_string(),
                    value: make_op_constant(spirv::Op::TypeVoid),
                }.into_node_ref(),
                NodeInstantiate {
                    node: NodeBlock {
                        nodes: vec![
                            NodeEmit {
                                instr: NodeInstr {
                                    opcode: NodeLookup {
                                        name: "foo".to_string(),
                                    }.into_node_ref(),
                                    operands: vec![],
                                    result_type: None,
                                    has_result: true,
                                }.into_node_ref(),
                            }.into_node_ref(),
                        ],
                        params: vec![],
                        result_node: None,
                    }.into_node_ref(),
                    args: vec![],
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&root).unwrap();
    let spirv = SpirvBinary::from_ir(x);

    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_argument_lookup() {
    let bar = NodeArg {
        name: "bar".to_string(),
    }.into_node_ref();
    let root = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                NodeDefine {
                    name: "foo".to_string(),
                    value: make_op_constant(spirv::Op::TypeVoid),
                }.into_node_ref(),
                NodeInstantiate {
                    node: NodeBlock {
                        nodes: vec![
                            NodeEmit {
                                instr: NodeInstr {
                                    opcode: bar.clone(),
                                    operands: vec![],
                                    result_type: None,
                                    has_result: true,
                                }.into_node_ref(),
                            }.into_node_ref(),
                        ],
                        params: vec![
                            bar.clone(),
                        ],
                        result_node: None,
                    }.into_node_ref(),
                    args: vec![
                        NodeLookup {
                            name: "foo".to_string(),
                        }.into_node_ref(),
                    ],
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&root).unwrap();
    let spirv = SpirvBinary::from_ir(x);

    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_var_load_store() {
    let var_ = NodeVariable {
        name: "foo".to_string(),
        is_mutable: false,
    }.into_node_ref();
    let global_ = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![
                var_.clone(),
                NodeStore {
                    variable: var_.clone(),
                    value: make_int_constant(0),
                }.into_node_ref(),
                NodeStore {
                    variable: var_.clone(),
                    value: make_int_constant(1),
                }.into_node_ref(),
                NodeEmit {
                    instr: NodeInstr {
                        opcode: make_op_constant(spirv::Op::TypeInt),
                        operands: vec![
                            make_int_constant(32),
                            NodeLoad {
                                variable: var_.clone(),
                            }.into_node_ref(),
                        ],
                        result_type: None,
                        has_result: true,
                    }.into_node_ref(),
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&global_).unwrap();
    let spirv = SpirvBinary::from_ir(x);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
"#.trim());
}

#[test]
fn test_increment_counter() {
    let global_ = NodeInstantiate {
        node: NodeBlock {
            nodes: vec![

                NodeEmit {
                    instr: NodeInstr {
                        opcode: make_op_constant(spirv::Op::TypeInt),
                        operands: vec![
                            NodeBinary {
                                binary_op: BinaryOp::Add,
                                lhs: make_int_constant(24),
                                rhs: make_int_constant(8),
                            }.into_node_ref(),
                            make_int_constant(1),
                        ],
                        result_type: None,
                        has_result: true,
                    }.into_node_ref(),
                }.into_node_ref(),
            ],
            params: vec![],
            result_node: None,
        }.into_node_ref(),
        args: vec![],
    }.into_node_ref();

    let x = l3ir::Lower::apply(&global_).unwrap();
    let spirv = SpirvBinary::from_ir(x);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
"#.trim());
}
