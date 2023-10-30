use anyhow::Result;
use crate::{Token, compiler::common::span::Span};
use super::{Parse, token::{Ident, BracketGroup}, ParseBuffer, Peek};

#[derive(Debug, Clone)]
pub struct Meta {
    pub hash: Token![#],
    pub bracket_group: BracketGroup,
    pub name: Ident,
    pub span: Span,
}
impl Parse for Meta {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let hash = input.parse::<Token![#]>()?;
        let bracket_group = input.parse::<BracketGroup>()?;
        let name = bracket_group.inner.clone().parse::<Ident>()?;

        let span = Span::join([
            hash.span(),
            bracket_group.span(),
            name.span(),
        ]);

        let out = Self {
            hash,
            bracket_group,
            name,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}
impl Peek for Meta {
    fn peek(input: &ParseBuffer) -> bool {
        input.peek::<Token![#]>()
    }
}

#[derive(Debug, Default, Clone)]
pub struct MetaList {
    pub metas: Vec<Meta>,
}
impl MetaList {
    pub fn get(&self, name: &str) -> Option<&Meta> {
        self.metas.iter().find(|meta| meta.name.name == name)
    }
    pub fn contains(&self, name: &str) -> bool {
        self.get(name).is_some()
    }
}
impl Parse for MetaList {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let mut metas = Vec::new();
        while Meta::peek(input) {
            let meta = input.parse::<Meta>()?;
            metas.push(meta);
        }

        let out = Self {
            metas,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        Span::join(self.metas.iter().map(|meta| meta.span()))
    }
}
