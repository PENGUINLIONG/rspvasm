use anyhow::Result;
use crate::{Token, compiler::common::span::Span};
use super::{stmt::Stmt, Parse, ParseBuffer, punctuated::Punctuated, token::Ident};

#[derive(Debug, Clone)]
pub struct BlockHeader {
    pub hline_token: Token![|],
    pub punctuated: Punctuated<Ident, Token![,]>,
    pub hline_token2: Token![|],
    pub span: Span,
}
impl Parse for BlockHeader {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let hline_token = input.parse::<Token![|]>()?;
        let punctuated = Punctuated::<Ident, Token![,]>::parse_separated_nonempty(input)?;
        let hline_token2 = input.parse::<Token![|]>()?;

        let out = Self {
            span: Span::join([hline_token.span(), punctuated.span(), hline_token2.span()]),
            hline_token,
            punctuated,
            hline_token2,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}
impl Parse for Block {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let mut stmts = Vec::new();
        while !input.is_empty() {
            let stmt = input.parse::<Stmt>()?;
            stmts.push(stmt);
        }
        let span = Span::join(stmts.iter().map(|stmt| stmt.span()));

        let out = Self { stmts, span };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}
impl FromIterator<Stmt> for Block {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        let stmts: Vec<Stmt> = iter.into_iter().collect();
        let span = Span::join(stmts.iter().map(|stmt| stmt.span()));

        Self {
            stmts,
            span,
        }
    }
}
impl From<Vec<Stmt>> for Block {
    fn from(stmts: Vec<Stmt>) -> Self {
        Block::from_iter(stmts)
    }
}
