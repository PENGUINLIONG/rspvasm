//! Descriptor resources.
use super::*;
use compiler::Compiler;
use pretty_assertions::assert_eq;

const HEADER: &'static str = r#"
const Op {
    Nop = 0,
    SourceContinued = 2,
    Source = 3,
    SourceExtension = 4,
    Name = 5,
    MemberName = 6,
    String = 7,
    Extension = 10,
    ExtInstImport = 11,
    MemoryModel = 14,
    EntryPoint = 15,
    ExecutionMode = 16,
    Capability = 17,
    TypeVoid = 19,
    TypeBool = 20,
    TypeFunction = 33,
    ConstantTrue = 41,
    Function = 54,
    FunctionEnd = 56,
    Decorate = 71,
    MemberDecorate = 72,
    DecorateGroup = 73,
    GroupDecorate = 74,
    GroupMemberDecorate = 75,
    SelectionMerge = 247,
    Label = 248,
    Branch = 249,
    BranchConditional = 250,
    Return = 253,
    Moduletemplateed = 330,
    ExecutionModeId = 331,
    DecorateId = 332,
    DecorateString = 5632,
    MemberDecorateString = 5633,
}
const Capability {
    Shader = 1,
}
const AddressingModel {
    Logical = 0,
}
const MemoryModel {
    GLSL450 = 1,
}
const ExecutionModel {
    GLCompute = 5,
}
const ExecutionMode {
    LocalSize = 17,
}
const FunctionControl {
    None = 0,
}
const SelectionControl {
    None = 0,
}
const StorageClass {
    Function = 5,
}

layout {
    Op::Capability = 1,
    Op::Extension = 2,
    Op::ExtInstImport = 3,
    Op::MemoryModel = 4,
    Op::EntryPoint = 5,
    Op::ExecutionMode = 6,
    Op::ExecutionModeId = 6,
    Op::String = 7.1,
    Op::SourceExtension = 7.1,
    Op::Source = 7.1,
    Op::SourceContinued = 7.1,
    Op::Name = 7.2,
    Op::MemberName = 7.2,
    Op::Moduletemplateed = 7.3,
    Op::Decorate = 8,
    Op::MemberDecorate = 8,
    Op::DecorateId = 8,
    Op::GroupDecorate = 8,
    Op::GroupMemberDecorate = 8,
    Op::DecorateGroup = 8,
    Op::DecorateString = 8,
    Op::MemberDecorateString = 8,
}

let glsl_450_compute_shader = {
    ~Op::Capability(Capability::Shader);
    let glsl_std_450_ = ~Op::ExtInstImport("GLSL.std.450") -> _;
    ~Op::MemoryModel(AddressingModel::Logical, MemoryModel::GLSL450);
    glsl_std_450_
};

let make_block = |body| {
    let label_ = ~Op::Label -> _;
    body();
    label_
};

let make_function = |body| {
    let void_ = ~Op::TypeVoid -> _;
    let function_void_ = ~Op::TypeFunction(void_) -> _;
    let function_ = ~Op::Function(FunctionControl::None, function_void_) -> void_;
    body();
    ~Op::FunctionEnd;

    function_
};

let make_entry_point = |function, group_size_x, group_size_y, group_size_z| {
    ~Op::EntryPoint(ExecutionModel::GLCompute, function, "main");
    ~Op::ExecutionMode(function, ExecutionMode::LocalSize, group_size_x, group_size_y, group_size_z);
};

let return = {
    ~Op::Return;
};
"#;

#[test]
fn test_if_then_else() {
    let code = HEADER.to_string()
        + r#"
let glsl_std_450_ = glsl_450_compute_shader();

let bool_ = ~Op::TypeBool() -> _;
let true_ = ~Op::ConstantTrue(bool_) -> _;

let make_if_then_else_block = |condition, then_branch, else_branch| {
    #[manual]
    let head_block = make_block(body: {});

    #[manual]
    let then_block = make_block(body: then_branch);

    #[manual]
    let else_block = make_block(body: else_branch);

    #[manual]
    let merge_block = make_block(body: {});

    head_block;
    ~Op::SelectionMerge(merge_block, SelectionControl::None);
    ~Op::BranchConditional(condition, then_block, else_block);
    then_block;
    ~Op::Branch(merge_block);
    else_block;
    ~Op::Branch(merge_block);
    merge_block;

    head_block
};

#[manual]
let main_ = make_function(body: {
    make_if_then_else_block(
        condition: true_,
        then_branch: {
        },
        else_branch: {
        }
    );
    return();
});

make_entry_point(
    function: main_,
    group_size_x: 16,
    group_size_y: 8,
    group_size_z: 1,
);
"#;

    let spirv = Compiler::compile(&code);

    let dis = spirv.disassemble();

    assert_eq!(
        dis,
        r#"
OpCapability Shader
%1 = OpExtInstImport "GLSL.std.450"
OpMemoryModel Logical GLSL450
OpEntryPoint GLCompute %4 "main"
OpExecutionMode %4 LocalSize 16 8 1
%2 = OpTypeVoid
%3 = OpTypeFunction %2
%4 = OpFunction %2 None %3
%5 = OpLabel
OpReturn
OpFunctionEnd
"#
        .trim_start()
    );
}
