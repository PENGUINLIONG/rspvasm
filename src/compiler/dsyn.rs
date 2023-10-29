use std::{collections::HashMap, ops::Index};
use super::{syn::{ParseBuffer, token::{TokenTree, Ident, Punct, Spacing, Literal}, Parse}, common::span::Span};
use anyhow::{bail, anyhow, Result};

#[derive(Debug)]
pub enum MatValue {
    None,
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
    List(Vec<MatValue>),
    Dict(HashMap<String, MatValue>),
}
impl MatValue {
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
    pub fn as_list(&self) -> Result<&Vec<MatValue>> {
        match self {
            Self::List(list) => Ok(list),
            _ => bail!("expected list"),
        }
    }
    pub fn as_dict(&self) -> Result<&HashMap<String, MatValue>> {
        match self {
            Self::Dict(dict) => Ok(dict),
            _ => bail!("expected dict"),
        }
    }

    pub fn get(&self, name: &str) -> Option<&MatValue> {
        match self {
            Self::Dict(dict) => dict.get(name),
            _ => None,
        }
    }
    pub fn resolve_path(&self, path: &str) -> Option<&MatValue> {
        if let Some((cur, remain)) = path.split_once("::") {
            let cur = self.get(cur)?;
            cur.resolve_path(remain)
        } else {
            self.get(path)
        }
    }
}

#[derive(Debug)]
pub enum ParType {
    AnyIdent,
    AnyLiteral,
    AnyPunct,
    Ident(String),
    Punct(char),
    Sequence(Vec<Par>),
    Optional(Box<Par>),
    Punctuated(Box<Par>, char),
}
impl ParType {
    pub fn into_named_par(self, key: &str) -> Par {
        Par {
            key: Some(key.to_string()),
            ty: self,
        }
    }
    pub fn into_par(self) -> Par {
        Par {
            key: None,
            ty: self,
        }
    }
}

#[derive(Debug)]
pub struct Par {
    key: Option<String>,
    ty: ParType,
}
impl From<ParType> for Par {
    fn from(ty: ParType) -> Self {
        ty.into_par()
    }
}

#[derive(Debug)]
pub struct ParseContext {
    fields: HashMap<String, MatValue>,
}
impl ParseContext {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }

    pub fn peek(&mut self, par_ty: &ParType, input: &mut ParseBuffer) -> bool {
        match par_ty {
            ParType::AnyIdent => input.peek::<Ident>(),
            ParType::AnyLiteral => input.peek::<Literal>(),
            ParType::AnyPunct => input.peek::<Punct>(),
            ParType::Ident(ident) => {
                input.clone()
                    .parse::<Ident>()
                    .map(|x| x.name == *ident)
                    .unwrap_or(false)
            },
            ParType::Punct(punct) => {
                input.clone()
                    .parse::<Punct>()
                    .map(|x| x.ch == *punct && x.spacing == Spacing::Alone)
                    .unwrap_or(false)
            },
            ParType::Sequence(sequence) => {
                if let Some(first) = sequence.first() {
                    self.peek(&first.ty, input)
                } else {
                    true
                }
            },
            ParType::Optional(_) => true,
            ParType::Punctuated(par, _) => {
                self.peek(&par.ty, input)
            },
        }
    }

    pub fn parse_impl(&mut self, par_ty: &ParType, input: &mut ParseBuffer) -> Result<MatValue> {
        let out = match par_ty {
            ParType::AnyIdent => {
                let ident = input.parse::<Ident>()?;
                MatValue::Ident(ident)
            },
            ParType::AnyLiteral => {
                let literal = input.parse::<Literal>()?;
                MatValue::Literal(literal)
            },
            ParType::AnyPunct => {
                let punct = input.parse::<Punct>()?;
                MatValue::Punct(punct)
            },
            ParType::Ident(ident) => {
                let ident2 = input.parse::<Ident>()?;
                if ident2.name != *ident {
                    bail!("unexpected ident: {:?}", ident2);
                }
                MatValue::Ident(ident2)
            },
            ParType::Punct(punct) => {
                let punct2 = input.parse::<Punct>()?;
                if punct2.ch != *punct || punct2.spacing != Spacing::Alone {
                    bail!("unexpected punct: {:?}", punct);
                }
                MatValue::Punct(punct2)
            }
            ParType::Sequence(sequence) => {
                let mut out = HashMap::new();
                for par in sequence {
                    let value = self.parse_impl(&par.ty, input)?;
                    if let Some(key) = &par.key {
                        out.insert(key.clone(), value);
                    }
                }
                MatValue::Dict(out)
            },
            ParType::Optional(par) => {
                if self.peek(&par.ty, input) {
                    self.parse_impl(&par.ty, input)?
                } else {
                    MatValue::None
                }
            },
            ParType::Punctuated(par, punct) => {
                let mut out = Vec::new();
                loop {
                    if !self.peek(&par.ty, input) {
                        break;
                    }

                    let mut ctxt = ParseContext::new();
                    ctxt.parse(par, input)?;
                    out.push(ctxt.into_mat());

                    if !self.peek(&ParType::Punct(*punct), input) {
                        break;
                    }

                    self.parse_impl(&ParType::Punct(*punct), input)?;
                }

                MatValue::List(out)
            },
        };

        Ok(out)
    }
    pub fn parse(&mut self, par: &Par, input: &mut ParseBuffer) -> Result<()> {
        let value = self.parse_impl(&par.ty, input)?;
        if let Some(key) = &par.key {
            self.fields.insert(key.clone(), value);
        }

        Ok(())
    }

    pub fn into_mat(self) -> MatValue {
        MatValue::Dict(self.fields)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::syn::token::Lit;

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_basic() {
        // Ident.
        {
            let mut input = ParseBuffer::from("a");
            let mut ctx = ParseContext::new();
    
            let par = Par {
                key: Some("first_ident".to_string()),
                ty: ParType::AnyIdent,
            };
            ctx.parse(&par, &mut input).unwrap();
            let mat = ctx.into_mat();

            let dict = mat.as_dict().unwrap();
            assert_eq!(dict.len(), 1);
            let value = dict.get("first_ident").unwrap();
    
            let ident = value.as_ident().unwrap();
            assert_eq!(ident.name, "a");
        }

        // Literal.
        {
            let mut input = ParseBuffer::from("1");
            let mut ctx = ParseContext::new();
    
            let par = Par {
                key: Some("first_literal".to_string()),
                ty: ParType::AnyLiteral,
            };
            ctx.parse(&par, &mut input).unwrap();
            let mat = ctx.into_mat();

            let dict = mat.as_dict().unwrap();
            assert_eq!(dict.len(), 1);
            let value = dict.get("first_literal").unwrap();
    
            let literal = value.as_literal().unwrap();
            assert_eq!(literal.lit, Lit::Int(1));
        }

        // Punct.
        {
            let mut input = ParseBuffer::from("=");
            let mut ctx = ParseContext::new();
    
            let par = Par {
                key: Some("first_punct".to_string()),
                ty: ParType::AnyPunct,
            };
            ctx.parse(&par, &mut input).unwrap();
            let mat = ctx.into_mat();

            let dict = mat.as_dict().unwrap();
            assert_eq!(dict.len(), 1);
            let value = dict.get("first_punct").unwrap();
    
            let punct = value.as_punct().unwrap();
            assert_eq!(punct.ch, '=');
            assert_eq!(punct.spacing, Spacing::Alone);
        }
    }

    #[test]
    fn test_parse_sequence() {
        let mut input = ParseBuffer::from("a = 1");
        let mut ctx = ParseContext::new();

        let par = ParType::Sequence(vec![
            ParType::AnyIdent.into_named_par("first_ident"),
            ParType::AnyPunct.into_named_par("first_punct"),
            ParType::AnyLiteral.into_named_par("first_literal"),
        ]).into_named_par("seq");
        ctx.parse(&par, &mut input).unwrap();
        let mat = ctx.into_mat();

        let value = mat.resolve_path("seq::first_ident").unwrap();
        let ident = value.as_ident().unwrap();
        assert_eq!(ident.name, "a");

        let value = mat.resolve_path("seq::first_punct").unwrap();
        let punct = value.as_punct().unwrap();
        assert_eq!(punct.ch, '=');
        assert_eq!(punct.spacing, Spacing::Alone);

        let value = mat.resolve_path("seq::first_literal").unwrap();
        let literal = value.as_literal().unwrap();
        assert_eq!(literal.lit, Lit::Int(1));
    }

    #[test]
    fn test_parse_nested_sequence() {
        let mut input = ParseBuffer::from("a = 1 b = 2");
        let mut ctx = ParseContext::new();

        let par = ParType::Sequence(vec![
            ParType::AnyIdent.into_named_par("first_ident"),
            ParType::AnyPunct.into_named_par("first_punct"),
            ParType::AnyLiteral.into_named_par("first_literal"),
            ParType::Sequence(vec![
                ParType::AnyIdent.into_named_par("second_ident"),
                ParType::AnyPunct.into_named_par("second_punct"),
                ParType::AnyLiteral.into_named_par("second_literal"),
            ]).into_named_par("second_seq"),
        ]).into_named_par("seq");

        ctx.parse(&par, &mut input).unwrap();
        let mat = ctx.into_mat();

        let value = mat.resolve_path("seq::first_ident").unwrap();
        let ident = value.as_ident().unwrap();
        assert_eq!(ident.name, "a");

        let value = mat.resolve_path("seq::first_punct").unwrap();
        let punct = value.as_punct().unwrap();
        assert_eq!(punct.ch, '=');
        assert_eq!(punct.spacing, Spacing::Alone);

        let value = mat.resolve_path("seq::first_literal").unwrap();
        let literal = value.as_literal().unwrap();
        assert_eq!(literal.lit, Lit::Int(1));

        let value = mat.resolve_path("seq::second_seq::second_ident").unwrap();
        let ident = value.as_ident().unwrap();
        assert_eq!(ident.name, "b");

        let value = mat.resolve_path("seq::second_seq::second_punct").unwrap();
        let punct = value.as_punct().unwrap();
        assert_eq!(punct.ch, '=');
        assert_eq!(punct.spacing, Spacing::Alone);

        let value = mat.resolve_path("seq::second_seq::second_literal").unwrap();
        let literal = value.as_literal().unwrap();
        assert_eq!(literal.lit, Lit::Int(2));
    }

    #[test]
    fn test_parse_punctuated() {
        let mut input = ParseBuffer::from("a = 1, b = 2");
        let mut ctx = ParseContext::new();

        let par = ParType::Punctuated(
            Box::new(
                ParType::Sequence(vec![
                    ParType::AnyIdent.into_named_par("first_ident"),
                    ParType::AnyPunct.into_named_par("first_punct"),
                    ParType::AnyLiteral.into_named_par("first_literal"),
                ]).into_named_par("seq")
            ),
            ','
        ).into_named_par("punctuated");

        ctx.parse(&par, &mut input).unwrap();
        let mat = ctx.into_mat();
        let list = mat.get("punctuated").unwrap().as_list().unwrap();
        assert_eq!(list.len(), 2);

        // First expression.
        {
            let mat = list.index(0);

            let value = mat.resolve_path("seq::first_ident").unwrap();
            let ident = value.as_ident().unwrap();
            assert_eq!(ident.name, "a");

            let value = mat.resolve_path("seq::first_punct").unwrap();
            let punct = value.as_punct().unwrap();
            assert_eq!(punct.ch, '=');
            assert_eq!(punct.spacing, Spacing::Alone);

            let value = mat.resolve_path("seq::first_literal").unwrap();
            let literal = value.as_literal().unwrap();
            assert_eq!(literal.lit, Lit::Int(1));
        }

        // Second expression.
        {
            let mat = list.index(1);

            let value = mat.resolve_path("seq::first_ident").unwrap();
            let ident = value.as_ident().unwrap();
            assert_eq!(ident.name, "b");

            let value = mat.resolve_path("seq::first_punct").unwrap();
            let punct = value.as_punct().unwrap();
            assert_eq!(punct.ch, '=');
            assert_eq!(punct.spacing, Spacing::Alone);

            let value = mat.resolve_path("seq::first_literal").unwrap();
            let literal = value.as_literal().unwrap();
            assert_eq!(literal.lit, Lit::Int(2));
        }

    }

    #[test]
    fn test_parse_exact_match() {
        let mut input = ParseBuffer::from("a = 1");
        let mut ctx = ParseContext::new();

        let par = ParType::Punctuated(
            Box::new(
                ParType::Sequence(vec![
                    ParType::Ident("a".to_string()).into_named_par("first_ident"),
                    ParType::Punct('=').into_named_par("first_punct"),
                    ParType::AnyLiteral.into_named_par("first_literal"),
                ]).into_named_par("seq")
            ),
            ','
        ).into_named_par("punctuated");

        ctx.parse(&par, &mut input).unwrap();
        let mat = ctx.into_mat();
        let list = mat.get("punctuated").unwrap().as_list().unwrap();
        assert_eq!(list.len(), 1);

        {
            let mat = list.index(0);

            let value = mat.resolve_path("seq::first_ident").unwrap();
            let ident = value.as_ident().unwrap();
            assert_eq!(ident.name, "a");

            let value = mat.resolve_path("seq::first_punct").unwrap();
            let punct = value.as_punct().unwrap();
            assert_eq!(punct.ch, '=');
            assert_eq!(punct.spacing, Spacing::Alone);

            let value = mat.resolve_path("seq::first_literal").unwrap();
            let literal = value.as_literal().unwrap();
            assert_eq!(literal.lit, Lit::Int(1));
        }
    }
}
