use std::collections::HashMap;
use super::{syn::{ParseBuffer, token::{TokenTree, Ident, Punct, Spacing, Literal}, Parse}, common::span::Span};
use anyhow::{bail, anyhow, Result};

#[derive(Debug)]
pub enum MatValue {
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
}
impl MatValue {
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
}

#[derive(Debug)]
pub enum ParType {
    AnyIdent,
    AnyLiteral,
    AnyPunct,
}
#[derive(Debug)]
pub struct Par {
    key: Option<String>,
    ty: ParType,
}

#[derive(Debug)]
pub struct ParseContextStack {
    prefix: String,
}

#[derive(Debug)]
pub struct ParseContext {
    fields: HashMap<String, MatValue>,
    stack: Vec<ParseContextStack>,
}
impl ParseContext {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
            stack: vec![
                ParseContextStack {
                    prefix: "".to_string(),
                },
            ],
        }
    }

    pub fn parse(&mut self, par: &Par, input: &mut ParseBuffer) -> Result<()> {
        let value = match par.ty {
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
        };

        if let Some(key) = &par.key {
            let prefix = self.stack.last()
                .expect("stack is empty")
                .prefix
                .as_str();
            let key = format!("{}::{}", prefix, key);
            self.fields.insert(key, value);
        }
        Ok(())
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
    
            assert_eq!(ctx.fields.len(), 1);
            let value = ctx.fields.get("::first_ident").unwrap();
    
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
    
            assert_eq!(ctx.fields.len(), 1);
            let value = ctx.fields.get("::first_literal").unwrap();
    
            let literal = value.as_literal().unwrap();
            assert_eq!(literal.lit, Lit::Int(1));
        }

        // Punct.
        {
            let mut input = ParseBuffer::from("+");
            let mut ctx = ParseContext::new();
    
            let par = Par {
                key: Some("first_punct".to_string()),
                ty: ParType::AnyPunct,
            };
            ctx.parse(&par, &mut input).unwrap();
    
            assert_eq!(ctx.fields.len(), 1);
            let value = ctx.fields.get("::first_punct").unwrap();
    
            let punct = value.as_punct().unwrap();
            assert_eq!(punct.ch, '+');
            assert_eq!(punct.spacing, Spacing::Alone);
        }
    }


}
