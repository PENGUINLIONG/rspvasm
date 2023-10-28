use std::collections::HashMap;
use super::{syn::{ParseBuffer, token::{TokenTree, Ident, Punct, Spacing, Literal}, Parse}, common::span::Span};
use anyhow::{bail, anyhow, Result};

#[derive(Debug)]
pub enum MatValue {
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
    Sequence(Vec<MatValue>),
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
    Sequence(Vec<Par>),
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
pub struct ParseContextStackFrame {
    prefix: Option<String>,
}

#[derive(Debug)]
pub struct ParseContext {
    fields: HashMap<String, MatValue>,
    stack: Vec<ParseContextStackFrame>,
}
impl ParseContext {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
            stack: vec![
                ParseContextStackFrame {
                    prefix: Some("".to_string()),
                },
            ],
        }
    }

    pub fn get_prefix(&self) -> Option<&str> {
        if let Some(last) = self.stack.last() {
            last.prefix.as_ref().map(|x| x.as_str())
        } else {
            None
        }
    }

    pub fn push_stack(&mut self, prefix: Option<String>) {
        self.stack.push(ParseContextStackFrame { prefix });
    }
    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    pub fn parse(&mut self, par: &Par, input: &mut ParseBuffer) -> Result<()> {
        let key = if let Some(key) = &par.key {
            if let Some(prefix) = self.get_prefix() {
                Some(format!("{}::{}", prefix, key))
            } else {
                None
            }
        } else {
            None
        };

        let value = match &par.ty {
            ParType::AnyIdent => {
                let ident = input.parse::<Ident>()?;
                Some(MatValue::Ident(ident))
            },
            ParType::AnyLiteral => {
                let literal = input.parse::<Literal>()?;
                Some(MatValue::Literal(literal))
            },
            ParType::AnyPunct => {
                let punct = input.parse::<Punct>()?;
                Some(MatValue::Punct(punct))
            },
            ParType::Sequence(sequence) => {
                self.push_stack(key.clone());
                for par in sequence {
                    self.parse(&par, input)?;
                }
                self.pop_stack();

                None
            },
            
        };

        if let Some((key, value)) = key.zip(value) {
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
        println!("{:#?}", ctx);

        assert_eq!(ctx.fields.len(), 3);

        let value = ctx.fields.get("::seq::first_ident").unwrap();
        let ident = value.as_ident().unwrap();
        assert_eq!(ident.name, "a");

        let value = ctx.fields.get("::seq::first_punct").unwrap();
        let punct = value.as_punct().unwrap();
        assert_eq!(punct.ch, '=');
        assert_eq!(punct.spacing, Spacing::Alone);

        let value = ctx.fields.get("::seq::first_literal").unwrap();
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
        println!("{:#?}", ctx);

        assert_eq!(ctx.fields.len(), 6);

        let value = ctx.fields.get("::seq::first_ident").unwrap();
        let ident = value.as_ident().unwrap();
        assert_eq!(ident.name, "a");

        let value = ctx.fields.get("::seq::first_punct").unwrap();
        let punct = value.as_punct().unwrap();
        assert_eq!(punct.ch, '=');
        assert_eq!(punct.spacing, Spacing::Alone);

        let value = ctx.fields.get("::seq::first_literal").unwrap();
        let literal = value.as_literal().unwrap();
        assert_eq!(literal.lit, Lit::Int(1));

        let value = ctx.fields.get("::seq::second_seq::second_ident").unwrap();
        let ident = value.as_ident().unwrap();
        assert_eq!(ident.name, "b");

        let value = ctx.fields.get("::seq::second_seq::second_punct").unwrap();
        let punct = value.as_punct().unwrap();
        assert_eq!(punct.ch, '=');
        assert_eq!(punct.spacing, Spacing::Alone);

        let value = ctx.fields.get("::seq::second_seq::second_literal").unwrap();
        let literal = value.as_literal().unwrap();
        assert_eq!(literal.lit, Lit::Int(2));
    }

}
