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
        let (x, layouts) = l3ir::Lower::apply(&x).unwrap();
        let mut ctxt = l1ir::Lower::apply(x).unwrap();
        ctxt.sort_by_layouts(layouts);
        let spirv = SpirvBinary::from_ir(ctxt);
        spirv
    }
}
