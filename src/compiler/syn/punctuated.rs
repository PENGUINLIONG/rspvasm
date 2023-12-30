use crate::compiler::common::span::{Span, SpanBuilder};
use anyhow::Result;

use super::token::*;
use super::{Parse, ParseBuffer};

#[derive(Debug, Clone)]
pub struct Punctuated<T, P> {
    pub items: Vec<(T, P)>,
    pub last: Option<T>,
    pub span: Span,
}
impl<T, P> Punctuated<T, P> {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            last: None,
            span: Span::call_site(),
        }
    }

    fn push_item(&mut self, item: T) {
        assert!(self.last.is_none(), "last is Some");
        self.last = Some(item);
    }
    fn push_punct(&mut self, punct: P) {
        assert!(self.last.is_some(), "last is None");
        let item = self.last.take().unwrap();
        self.items.push((item, punct));
        self.last = None;
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items
            .iter()
            .map(|(item, _)| item)
            .chain(self.last.iter())
    }

    pub fn parse_terminated(input: &mut ParseBuffer) -> Result<Self>
    where
        T: Parse,
        P: Parse,
    {
        let mut out = Self::new();

        let mut sb = SpanBuilder::new();
        loop {
            if input.is_empty() {
                break;
            }
            let item = input.parse::<T>()?;
            sb.push(item.span());
            out.push_item(item);
            if input.is_empty() {
                break;
            }
            let punct = input.parse::<P>()?;
            sb.push(punct.span());
            out.push_punct(punct);
        }

        out.span = sb.into();
        Ok(out)
    }

    pub fn parse_separated_nonempty(input: &mut ParseBuffer) -> Result<Self>
    where
        T: Parse,
        P: Parse + Token,
    {
        let mut out = Self::new();

        let mut sb = SpanBuilder::new();
        loop {
            let item = input.parse::<T>()?;
            sb.push(item.span());
            out.push_item(item);
            if !P::peek(input) {
                break;
            }
            let punct = input.parse::<P>()?;
            sb.push(punct.span());
            out.push_punct(punct);
        }

        out.span = sb.into();
        Ok(out)
    }
}
impl<T, P> Parse for Punctuated<T, P>
where
    T: Parse,
    P: Parse,
{
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        Self::parse_terminated(input)
    }

    fn span(&self) -> Span {
        self.span
    }
}
