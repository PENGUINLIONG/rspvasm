use pretty_assertions::assert_eq;

use super::*;

fn disassemble_spirv(spirv: &[u32]) -> String {
    use spirq_spvasm::{Disassembler, SpirvBinary};
    let spv = SpirvBinary::from(spirv);
    let spvasm = Disassembler::new()
        .indent(false)
        .print_header(false)
        .disassemble(&spv)
        .unwrap();
    spvasm
}

#[test]
fn test_empty_spirv() {
    let ctxt = InstrContext::default();
    let spirv = SpirvBinary::from(ctxt);
    assert_eq!(spirv.to_words(), vec![0x07230203, 0x00010300, 0, 0, 0]);
}

#[test]
fn test_op_no_result() {
    let mut ctxt = InstrContext::default();
    ctxt.build_instr(spirv::Op::MemoryModel as u16)
        .set_operands(vec![
            spirv::AddressingModel::Logical as u32,
            spirv::MemoryModel::GLSL450 as u32,
        ])
        .build();

    let spirv = SpirvBinary::from(ctxt);
    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(
        dis,
        r#"
OpMemoryModel Logical GLSL450
"#
        .trim()
    );
}

#[test]
fn test_op_with_result() {
    let mut ctxt = InstrContext::default();
    let result = ctxt
        .build_instr(spirv::Op::TypeVoid as u16)
        .build_with_result();

    assert_eq!(result.get(), 1);

    let spirv = SpirvBinary::from(ctxt);
    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(
        dis,
        r#"
%1 = OpTypeVoid
"#
        .trim()
    );
}

#[test]
fn test_op_with_result_type() {
    let mut ctxt = InstrContext::default();
    let type_int = ctxt
        .build_instr(spirv::Op::TypeInt as u16)
        .set_operands(vec![32, 1])
        .build_with_result();
    let _constant_1 = ctxt
        .build_instr(spirv::Op::Constant as u16)
        .set_result_type(&type_int)
        .set_operands(vec![1])
        .build_with_result();

    let spirv = SpirvBinary::from(ctxt);
    let dis = disassemble_spirv(&spirv.to_words());
    assert_eq!(
        dis,
        r#"
%1 = OpTypeInt 32 1
%2 = OpConstant  %1  1
"#
        .trim()
    );
}
