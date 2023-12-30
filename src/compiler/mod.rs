mod common;
mod dsyn;
mod l0ir;
mod l3ir;
mod syn;

pub use l0ir::SpirvBinary;
use syn::{lower::LowerToAst, stmt::parse_stmts, ParseBuffer};

pub struct Compiler {}
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
