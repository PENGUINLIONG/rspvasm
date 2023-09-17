use anyhow::Result;
use crate::Token;
use super::{stmt::Stmt, Parse, ParseBuffer, punctuated::Punctuated, token::Ident};

#[derive(Debug, Clone)]
pub struct BlockHeader {
    pub hline_token: Token![|],
    pub punctuated: Punctuated<Ident, Token![,]>,
    pub hline_token2: Token![|],
}
impl Parse for BlockHeader {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let hline_token = input.parse::<Token![|]>()?;
        let punctuated = Punctuated::<Ident, Token![,]>::parse_separated_nonempty(input)?;
        let hline_token2 = input.parse::<Token![|]>()?;

        let out = Self {
            hline_token,
            punctuated,
            hline_token2,
        };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}
impl Parse for Block {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let mut stmts = Vec::new();
        while !input.is_empty() {
            let stmt = input.parse::<Stmt>()?;
            stmts.push(stmt);
        }
        let out = Self { stmts };
        Ok(out)
    }
}
impl FromIterator<Stmt> for Block {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        let stmts = iter.into_iter().collect();
        Self { stmts }
    }
}
impl From<Vec<Stmt>> for Block {
    fn from(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}
