use anyhow::Result;
use crate::Token;

use super::{ token::*, path::Path };
use super::{ Parse, ParseBuffer };

#[derive(Debug, Clone)]
pub struct PatLiteral {
    pub literal: Literal,
}
impl Parse for PatLiteral {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let literal = input.parse::<Literal>()?;
        Ok(Self { literal })
    }
}

#[derive(Debug, Clone)]
pub struct PatPath {
    pub path: Path,
}
impl Parse for PatPath {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let path = input.parse::<Path>()?;
        Ok(Self { path })
    }
}

#[derive(Debug, Clone)]
pub struct PatUnderscore {
    pub underscore_token: Token![_],
}
impl Parse for PatUnderscore {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let underscore_token = input.parse::<Token![_]>()?;
        Ok(Self { underscore_token })
    }
}

#[derive(Debug, Clone)]
pub enum Pat {
    Literal(PatLiteral),
    Path(PatPath),
    Underscore(PatUnderscore),
}
impl Parse for Pat {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        if input.peek::<Literal>() {
            let literal = input.parse::<PatLiteral>()?;
            Ok(Self::Literal(literal))
        } else if input.peek::<Token![_]>() {
            let underscore = input.parse::<PatUnderscore>()?;
            Ok(Self::Underscore(underscore))
        } else {
            let path = input.parse::<PatPath>()?;
            Ok(Self::Path(path))
        }
    }
}
