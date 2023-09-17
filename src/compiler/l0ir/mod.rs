//! Level-0 Intermediate Representation
//!
//! Fully flattened one-to-one mapping to SPIR-V instructions. No nested
//! language structure and ID references are concrete numbers.
use std::{collections::HashMap, rc::Rc};

use ordered_float::OrderedFloat;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdToken(Rc<u32>);
impl IdToken {
    pub fn new(id: u32) -> Self {
        Self(Rc::new(id))
    }

    pub fn get(&self) -> u32 {
        *self.0
    }
}

#[derive(Debug, Clone)]
pub struct IdContext {
    counter: u32,
    id_tokens: Vec<IdToken>,
}
impl IdContext {
    pub fn new() -> Self {
        Self {
            counter: 1,
            id_tokens: Vec::new(),
        }
    }

    pub fn alloc(&mut self) -> IdToken {
        let id = self.counter;
        self.counter += 1;
        let token = IdToken::new(id);
        self.id_tokens.push(token.clone());
        token
    }
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub opcode: u16,
    pub result_type: Option<u32>,
    pub result_id: Option<u32>,
    pub operands: Vec<u32>,
}
impl Instr {
    pub fn to_words(&self) -> Vec<u32> {
        let mut out = Vec::new();
        out.push(self.opcode as u32);
        if let Some(result_type) = self.result_type {
            out.push(result_type);
        }
        if let Some(result_id) = self.result_id {
            out.push(result_id);
        }
        out.extend(self.operands.iter().cloned());
        let len = out.len() as u32;
        assert!(len <= u16::MAX as u32, "instruction length must be less than u16::MAX (65535)");
        out[0] |= len << 16;
        out
    }
}

pub struct InstrBuilder<'a> {
    ctxt: &'a mut InstrContext,
    opcode: u16,
    result_type: Option<u32>,
    result_id: Option<IdToken>,
    operands: Vec<u32>,
}
impl<'a> InstrBuilder<'a> {
    pub fn new(ctxt: &'a mut InstrContext, opcode: u16) -> Self {
        Self {
            ctxt,
            opcode,
            result_type: None,
            result_id: None,
            operands: Vec::new(),
        }
    }

    pub fn set_result_type(&mut self, result_type: &IdToken) -> &mut Self {
        self.result_type = Some(result_type.get());
        self
    }
    pub fn set_result_id(&mut self, result_id: &IdToken) -> &mut Self {
        self.result_id = Some(result_id.clone());
        self
    }
    #[cfg(test)]
    pub fn alloc_result_id(&mut self) -> &mut Self {
        let id_token = self.ctxt.alloc_id();
        self.set_result_id(&id_token);
        self
    }
    pub fn set_operands(&mut self, operands: Vec<u32>) -> &mut Self {
        self.operands = operands;
        self
    }

    pub fn build(&mut self) -> Option<IdToken> {
        let id = self.result_id.as_ref().map(|x| x.get());
        let instr = Instr {
            opcode: self.opcode,
            result_type: self.result_type,
            result_id: id,
            operands: self.operands.clone(),
        };
        self.ctxt.instrs.push(instr);
        self.result_id.clone()
    }
    #[cfg(test)]
    pub fn build_with_result(&mut self) -> IdToken {
        if self.result_id.is_none() {
            self.alloc_result_id();
        }
        self.build().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct SpirvHeader {
    pub magic: u32,
    pub version: u32,
    pub generator: u32,
    pub bound: u32,
    pub schema: u32,
}
impl SpirvHeader {
    pub fn new(version: u32, bound: u32) -> Self {
        Self {
            magic: 0x07230203,
            version,
            generator: 0,
            bound,
            schema: 0,
        }
    }

    pub fn to_words(&self) -> Vec<u32> {
        vec![
            self.magic,
            self.version,
            self.generator,
            self.bound,
            self.schema,
        ]
    }
}


#[derive(Debug, Clone)]
pub struct InstrContext {
    pub version: u32,
    pub counter: u32,
    pub instrs: Vec<Instr>,
    pub id_ctxt: IdContext,
}
impl InstrContext {
    pub fn new() -> InstrContext {
        InstrContext {
            counter: 1,
            version: 0x00010300,
            instrs: Vec::new(),
            id_ctxt: IdContext::new(),
        }
    }

    pub fn alloc_id(&mut self) -> IdToken {
        self.id_ctxt.alloc()
    }
    pub fn build_instr(&mut self, opcode: u16) -> InstrBuilder<'_> {
        InstrBuilder::new(self, opcode)
    }

    pub fn sort_by_layouts(&mut self, layouts: HashMap<u32, f32>) {
        self.instrs.sort_by_key(|x| {
            let position = layouts.get(&(x.opcode as u32))
                .unwrap_or(&f32::MAX);
            OrderedFloat(*position)
        });
    }

    /*
    pub fn from_ast(ast: ir::AstProgram) -> Result<InstrContext> {
        LowerToSpirv::lower(ast)
    }
    */
}
impl Default for InstrContext {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SpirvBinary {
    pub header: SpirvHeader,
    pub instrs: Vec<Instr>,
}
impl SpirvBinary {
    pub fn from_ir(instr_ctxt: InstrContext) -> Self {
        Self {
            header: SpirvHeader::new(instr_ctxt.version, instr_ctxt.counter - 1),
            instrs: instr_ctxt.instrs,
        }
    }

    pub fn to_words(&self) -> Vec<u32> {
        let mut out = Vec::new();
        out.extend(self.header.to_words());
        for instr in self.instrs.iter() {
            out.extend(instr.to_words());
        }
        out
    }

    #[cfg(test)]
    pub(crate) fn disassemble(&self) -> String {
        use rspirv::binary::Disassemble;
        let words = self.to_words();

        let mut d = rspirv::dr::Loader::new();
        rspirv::binary::parse_words(words, &mut d).unwrap();
        let mut module = d.module();
        module.header = None;
        module.disassemble()
    }
}
impl From<InstrContext> for SpirvBinary {
    fn from(ir: InstrContext) -> Self {
        Self::from_ir(ir)
    }
}
