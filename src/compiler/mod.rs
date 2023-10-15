mod common;
mod l0ir;
mod l1ir;
mod l3ir;
mod syn;

pub use l0ir::SpirvBinary;
use syn::{ParseBuffer, stmt::parse_stmts, lower::LowerToAst};

pub struct Compiler {
}
impl Compiler {
    pub fn compile(code: &str) -> SpirvBinary {
        let mut input = ParseBuffer::from(code.as_ref());
        let stmts = parse_stmts(&mut input).unwrap();
        let x = LowerToAst::apply(&stmts.into()).unwrap();
        let x = l3ir::Lower::apply(&x).unwrap();
        let spirv = SpirvBinary::from_ir(x);
        spirv
    }
}
