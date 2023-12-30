use crate::compiler::common::span::Span;
use crate::Token;
use anyhow::Result;

use super::{path::Path, token::*};
use super::{Parse, ParseBuffer};

#[derive(Debug, Clone)]
pub struct PatLiteral {
    pub literal: Literal,
    pub span: Span,
}
impl Parse for PatLiteral {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let literal = input.parse::<Literal>()?;
        let span = literal.span();

        let out = Self { literal, span };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct PatPath {
    pub path: Path,
    pub span: Span,
}
impl Parse for PatPath {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let path = input.parse::<Path>()?;
        let span = path.span;

        let out = Self { path, span };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct PatUnderscore {
    pub underscore_token: Token![_],
    pub span: Span,
}
impl Parse for PatUnderscore {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let underscore_token = input.parse::<Token![_]>()?;
        let span = underscore_token.span;

        Ok(Self {
            underscore_token,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
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

    fn span(&self) -> Span {
        match self {
            Self::Literal(literal) => literal.span(),
            Self::Path(path) => path.span(),
            Self::Underscore(underscore) => underscore.span(),
        }
    }
}
