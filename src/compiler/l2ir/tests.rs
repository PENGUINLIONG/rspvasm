use super::*;
use crate::compiler::{l1ir, l2ir, l0ir::SpirvBinary};

use pretty_assertions::assert_eq;

fn make_int_constant(x: u32) -> NodeRef {
    NodeConstant {
        value: ConstantValue::Int(x),
    }.into_node_ref()
}
fn make_op_constant(opcode: spirv::Op) -> NodeRef {
    make_int_constant(opcode as u32)
}
fn make_instr(opcode: spirv::Op, operands: Vec<NodeRef>, result_type: Option<NodeRef>, has_result: bool) -> NodeRef {
    NodeInstr {
        opcode: make_op_constant(opcode),
        operands,
        result_type,
        has_result,
    }.into_node_ref()
}


#[test]
fn test_instr() {
    let op_type_void_ = NodeConstant {
        value: ConstantValue::Int(spirv::Op::TypeVoid as u32),
    }.into_node_ref();
    let void_ = NodeInstr {
        opcode: op_type_void_,
        operands: vec![],
        result_type: None,
        has_result: true,
    }.into_node_ref();
    let global_ = NodeBlock {
        nodes: vec![
            void_.clone(),
        ],
    }.into_node_ref();

    let x = l2ir::Lower::apply(&global_).unwrap();
    let x = l1ir::Lower::apply(x).unwrap();
    let spirv = SpirvBinary::from_ir(x);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeVoid
"#.trim());
}

#[test]
fn test_nested_instrs() {
    let int_ = make_instr(spirv::Op::TypeInt, vec![
        make_int_constant(32),
        make_int_constant(1),
    ], None, true);
    let struct_int_ = make_instr(spirv::Op::TypeStruct, vec![
        int_.clone(),
    ], None, true);
    let pointer_struct_int_ = make_instr(spirv::Op::TypePointer, vec![
        make_int_constant(spirv::StorageClass::Uniform as u32),
        struct_int_.clone(),
    ], None, true);
    let variable_ = make_instr(spirv::Op::Variable, vec![
        make_int_constant(spirv::StorageClass::Uniform as u32),
    ], Some(pointer_struct_int_.clone()), true);
    let global_ = NodeBlock {
        nodes: vec![
            int_.clone(),
            struct_int_.clone(),
            pointer_struct_int_.clone(),
            variable_.clone(),
        ],
    }.into_node_ref();

    let x = l2ir::Lower::apply(&global_).unwrap();
    let x = l1ir::Lower::apply(x).unwrap();
    let spirv = SpirvBinary::from_ir(x);
    let dis = spirv.disassemble();
    assert_eq!(dis, r#"
%1 = OpTypeInt 32 1
%2 = OpTypeStruct %1
%3 = OpTypePointer Uniform %2
%4 = OpVariable  %3  Uniform
"#.trim());
}
