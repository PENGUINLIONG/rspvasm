use crate::compiler::{
    common::span::Span,
    syn::{
        token::{Ident, Literal, Punct, Spacing, TokenTree},
        Parse, ParseBuffer,
    },
};
use anyhow::{anyhow, bail, Result};
use std::{collections::HashMap, ops::Index};

#[derive(Debug)]
pub enum Match {
    None,
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
    List(Vec<Match>),
    Dict(HashMap<String, Match>),
}
impl Match {
    pub fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            _ => false,
        }
    }

    pub fn as_ident(&self) -> Result<&Ident> {
        match self {
            Self::Ident(ident) => Ok(ident),
            _ => bail!("expected ident"),
        }
    }
    pub fn as_literal(&self) -> Result<&Literal> {
        match self {
            Self::Literal(literal) => Ok(literal),
            _ => bail!("expected literal"),
        }
    }
    pub fn as_punct(&self) -> Result<&Punct> {
        match self {
            Self::Punct(punct) => Ok(punct),
            _ => bail!("expected punct"),
        }
    }
    pub fn as_list(&self) -> Result<&Vec<Match>> {
        match self {
            Self::List(list) => Ok(list),
            _ => bail!("expected list"),
        }
    }
    pub fn as_dict(&self) -> Result<&HashMap<String, Match>> {
        match self {
            Self::Dict(dict) => Ok(dict),
            _ => bail!("expected dict"),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Match> {
        match self {
            Self::Dict(dict) => dict.get(name),
            _ => None,
        }
    }
    pub fn resolve_path(&self, path: &str) -> Option<&Match> {
        if let Some((cur, remain)) = path.split_once("::") {
            let cur = self.get(cur)?;
            cur.resolve_path(remain)
        } else {
            self.get(path)
        }
    }
}
