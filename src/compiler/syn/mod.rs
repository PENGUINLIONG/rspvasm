use std::{rc::Rc, fmt};

use anyhow::Result;

pub mod token;
pub mod punctuated;
pub mod expr;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod block;
pub mod lower;

pub trait Parse {
    fn parse(input: &mut ParseBuffer) -> Result<Self>
    where
        Self: Sized;
}
impl<P: Parse> Parse for Vec<P> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let mut out = Vec::new();
        while let Ok(p) = input.parse::<P>() {
            out.push(p);
        }
        Ok(out)
    }
}
impl<P: Parse> Parse for Option<P> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        match input.parse::<P>() {
            Ok(p) => Ok(Some(p)),
            Err(_) => Ok(None),
        }
    }
}
impl Parse for () {
    fn parse(_input: &mut ParseBuffer) -> Result<Self> {
        Ok(())
    }
}
impl<P0: Parse, P1: Parse> Parse for (P0, P1) {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let p0 = input.parse::<P0>()?;
        let p1 = input.parse::<P1>()?;
        Ok((p0, p1))
    }
}
impl<P0: Parse, P1: Parse, P2: Parse> Parse for (P0, P1, P2) {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let p0 = input.parse::<P0>()?;
        let p1 = input.parse::<P1>()?;
        let p2 = input.parse::<P2>()?;
        Ok((p0, p1, p2))
    }
}
impl<P: Parse> Parse for Box<P> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let p = input.parse::<P>()?;
        Ok(Box::new(p))
    }
}

pub trait Peek {
    fn peek(input: &ParseBuffer) -> bool;
}
impl<P: Peek> Peek for Vec<P> {
    fn peek(input: &ParseBuffer) -> bool {
        P::peek(input)
    }
}
impl<P: Peek> Peek for Option<P> {
    fn peek(_input: &ParseBuffer) -> bool {
        true
    }
}
impl Peek for () {
    fn peek(_input: &ParseBuffer) -> bool {
        true
    }
}
impl<P0: Peek, P1: Peek> Peek for (P0, P1) {
    fn peek(input: &ParseBuffer) -> bool {
        P0::peek(input)
    }
}
impl<P0: Peek, P1: Peek, P2: Peek> Peek for (P0, P1, P2) {
    fn peek(input: &ParseBuffer) -> bool {
        P0::peek(input)
    }
}
impl<P: Peek> Peek for Box<P> {
    fn peek(input: &ParseBuffer) -> bool {
        P::peek(input)
    }
}

pub struct ParseBuffer {
    code: Rc<String>,
    pos: usize, // in bytes
    end: usize, // in bytes
}
impl ParseBuffer {
    pub fn new(code: &str) -> Self {
        let mut out = Self {
            end: code.len(),
            code: Rc::new(code.to_owned()),
            pos: 0,
        };
        out.trim_start();
        out
    }
    pub fn is_empty(&self) -> bool {
        self.pos >= self.end
    }
    fn trim_start(&mut self) {
        // Skip whitespace chars.
        let s = self.as_ref();
        let offset = s.bytes()
            .take_while(u8::is_ascii_whitespace)
            .count();
        self.pos += offset;
    }

    pub fn advance_n(&mut self, n: usize) {
        if self.pos + n > self.end {
            self.pos = self.end;
        } else {
            self.pos += n;
        }
        self.trim_start()
    }

    pub fn parse<P: Parse>(&mut self) -> Result<P> {
        P::parse(self)
    }
    pub fn peek<P: Peek>(&self) -> bool {
        P::peek(self)
    }

    pub fn slice(&self, start: usize, end: usize) -> ParseBuffer {
        assert!(start <= end);
        let code = Rc::clone(&self.code);
        let pos = (self.pos + start).min(self.end);
        let end = (self.pos + end).min(self.end);

        let mut out = Self { code, pos, end };
        out.trim_start();
        out
    }

    pub fn surrounding(&self) -> String {
        let extent = 20;
        let from = self.pos.saturating_sub(extent);
        let to = self.end.min(self.pos + extent);
        self.code[from..self.pos].to_owned() + "#HERE#" + &self.code[self.pos..to]
    }
}
impl From<&str> for ParseBuffer {
    fn from(code: &str) -> Self {
        Self::new(code)
    }
}
impl AsRef<str> for ParseBuffer {
    fn as_ref(&self) -> &str {
        &self.code[self.pos..self.end]
    }
}
impl fmt::Debug for ParseBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.code[self.pos..self.end])
    }
}
