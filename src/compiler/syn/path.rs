use anyhow::Result;
use super::{ token::*, punctuated::Punctuated };
use crate::Token;
use super::{ Parse, ParseBuffer };

#[derive(Debug, Clone)]
pub struct Path {
    pub colon_colon_token: Option<Token![::]>,
    pub segments: Punctuated<Ident, Token![::]>,
}
impl Parse for Path {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let colon_colon_token = if input.peek::<Token![::]>() {
            Some(input.parse::<Token![::]>()?)
        } else {
            None
        };
        let segments = Punctuated::<Ident, Token![::]>::parse_separated_nonempty(input)?;

        let out = Self { colon_colon_token, segments };
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path() {
        let mut input = ParseBuffer::from("::foo::bar::baz");
        let path = input.parse::<Path>().unwrap();
        println!("{:?}", path);
    }
}
