use super::{Parse, ParseBuffer, Peek};
use anyhow::{anyhow, Result};

macro_rules! unexpected_seq {
    ($expected:expr, $parse_buf:expr) => {
        {
            let surrounding = $parse_buf.surrounding();
            anyhow!("expected `{}`, surrounding is `{}`", $expected, surrounding)
        }
    };
}

pub trait Token : Peek {}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenTree {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}
impl Parse for TokenTree {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        if let Some(c) = input.as_ref().chars().next() {
            if c.is_ascii_digit() {
                return Ok(TokenTree::Literal(input.parse::<Literal>()?));
            } 
            if c.is_ascii_alphabetic() || c == '_' {
                return Ok(TokenTree::Ident(input.parse::<Ident>()?));
            }
            return Ok(TokenTree::Punct(input.parse::<Punct>()?));
        } else {
            Err(anyhow!("unexpected end of input"))
        }
    }
}
impl Peek for TokenTree {
    fn peek(input: &ParseBuffer) -> bool {
        let s: &str = input.as_ref();
        if let Some(c) = s.chars().next() {
            c.is_ascii_alphanumeric() || c == '_' || !c.is_ascii_whitespace()
        } else {
            false
        }
    }
}


macro_rules! define_token {
    ($( $name:literal => $ty:ident )*) => {
        $(
            #[derive(Debug, Default, PartialEq, Clone)]
            pub struct $ty {
            }
            impl Token for $ty {}
            impl Parse for $ty {
                fn parse(input: &mut ParseBuffer) -> Result<Self> {
                    let s: &str = input.as_ref();
                    if s.starts_with($name) {
                        input.advance_n($name.len());
                        Ok(Self::default())
                    } else {
                        Err(unexpected_seq!($name, input))
                    }
                }
            }
            impl Peek for $ty {
                fn peek(input: &ParseBuffer) -> bool {
                    let s: &str = input.as_ref();
                    if s.starts_with($name) {
                        if $name.chars().next().unwrap().is_ascii_alphabetic() {
                            if let Some(next) = s[$name.len()..].chars().next() {
                                if next.is_ascii_alphanumeric() || next == '_' {
                                    return false;
                                } else {
                                    return true;
                                }
                            } else {
                                return true;
                            }
                        } else {
                            return true;
                        }
                    } else {
                        return false;
                    }
                }
            }
        )*
    };
}

define_token! {
    "let" => Let
    "type" => Type
    "const" => Const
    "struct" => Struct
    "impl" => Impl
    "layout" => Layout
    "=" => Eq
    ";" => Semi
    ":" => Colon
    "::" => ColonColon
    "~" => Tilde
    "," => Comma
    "->" => Arrow
    "@" => At
    "_" => Underscore
    "|" => Hline
}

#[macro_export]
macro_rules! Token {
    [let] => { $crate::compiler::syn::token::Let };
    [type] => { $crate::compiler::syn::token::Type };
    [const] => { $crate::compiler::syn::token::Const };
    [struct] => { $crate::compiler::syn::token::Struct };
    [impl] => { $crate::compiler::syn::token::Impl };
    [layout] => { $crate::compiler::syn::token::Layout };
    [=] => { $crate::compiler::syn::token::Eq };
    [;] => { $crate::compiler::syn::token::Semi };
    [:] => { $crate::compiler::syn::token::Colon };
    [::] => { $crate::compiler::syn::token::ColonColon };
    [~] => { $crate::compiler::syn::token::Tilde };
    [,] => { $crate::compiler::syn::token::Comma };
    [->] => { $crate::compiler::syn::token::Arrow };
    [@] => { $crate::compiler::syn::token::At };
    [_] => { $crate::compiler::syn::token::Underscore };
    [|] => { $crate::compiler::syn::token::Hline };
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub name: String,
}
impl Token for Ident {}
impl Parse for Ident {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let s: &str = input.as_ref();
        let mut buf = String::new();
        for c in s.chars() {
            if c.is_ascii_alphanumeric() || c == '_' {
                buf.push(c);
            } else {
                break;
            }
        }
        if buf.is_empty() {
            Err(unexpected_seq!("identifier", input))
        } else {
            input.advance_n(buf.len());
            Ok(Self { name: buf })
        }
    }
}
impl Peek for Ident {
    fn peek(input: &ParseBuffer) -> bool {
        let s: &str = input.as_ref();
        if let Some(c) = s.chars().next() {
            c.is_ascii_alphabetic() || c == '_'
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    Alone,
    Joint,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Punct {
    pub ch: char,
    pub spacing: Spacing,
}
impl Token for Punct {}
impl Parse for Punct {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let s: &str = input.as_ref();
        let mut chars = s.chars();
        match chars.next() {
            Some(c) if !c.is_ascii_alphanumeric() && c != '_' => {
                let spacing = if let Some(c2) = chars.next() {
                    if (c2.is_ascii_alphanumeric() || c2 == '_') && c2.is_ascii_whitespace() {
                        Spacing::Joint
                    } else {
                        Spacing::Alone
                    }
                } else {
                    Spacing::Alone
                };
                input.advance_n(c.len_utf8());
                Ok(Self { ch: c, spacing })
            }
            _ => {
                return Err(unexpected_seq!("punctuation", input));
            }
        }
    }
}
impl Peek for Punct {
    fn peek(input: &ParseBuffer) -> bool {
        let s: &str = input.as_ref();
        if let Some(c) = s.chars().next() {
            !c.is_ascii_alphanumeric() && c != '_'
        } else {
            false
        }
    }
}

fn try_parse_group<T: Parse>(
    input: &mut ParseBuffer,
    open_char: &str,
    close_char: &str,
) -> Result<T> {
    let s = input.as_ref();
    if s.starts_with(open_char) {
        // Workaround nesting groups.
        let mut next_close = s[1..].find(close_char).map(|x| x + 1);
        let mut next_open = s[1..].find(open_char).map(|x| x + 1);
        let mut depth = 0;
        while let (Some(iclose), Some(iopen)) = (next_close, next_open) {
            if iopen < iclose {
                // Nested group.
                depth += 1;
                next_open = s[iopen + 1..].find(open_char).map(|x| x + iopen + 1);
            } else {
                // Close group.
                if depth == 0 {
                    next_close = Some(iclose);
                    break;
                } else {
                    depth -= 1;
                    next_close = s[iclose + 1..].find(close_char).map(|x| x + iclose + 1);
                }
            }
        }

        while let Some(iclose) = next_close {
            if depth == 0 {
                let mut content = input.slice(1, iclose);
                let tokens = T::parse(&mut content)?;
                input.advance_n(iclose + 1);
                return Ok(tokens);
            } else {
                depth -= 1;
                next_close = s[iclose + 1..].find(close_char).map(|x| x + iclose + 1);
            }
        }
        Err(unexpected_seq!(close_char, input))
    } else {
        Err(unexpected_seq!(open_char, input))
    }
}
fn try_peek_group<T: Peek>(
    input: &ParseBuffer,
    open_char: &str,
) -> bool {
    let s: &str = input.as_ref();
    if let Some(c) = s.chars().next() {
        c == open_char.chars().next().unwrap()
    } else {
        false
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParenGroup<T>(pub T);
impl<T: Parse> Parse for ParenGroup<T> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let inner = try_parse_group::<T>(input, "(", ")")?;
        Ok(Self(inner))
    }
}
impl<T: Peek> Peek for ParenGroup<T> {
    fn peek(input: &ParseBuffer) -> bool {
        try_peek_group::<T>(input, "(")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BracketGroup<T>(pub T);
impl<T: Parse> Parse for BracketGroup<T> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let inner = try_parse_group::<T>(input, "[", "]")?;
        Ok(Self(inner))
    }
}
impl<T: Peek> Peek for BracketGroup<T> {
    fn peek(input: &ParseBuffer) -> bool {
        try_peek_group::<T>(input, "[")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BraceGroup<T>(pub T);
impl<T: Parse> Parse for BraceGroup<T> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let inner = try_parse_group::<T>(input, "{", "}")?;
        Ok(Self(inner))
    }
}
impl<T: Peek> Peek for BraceGroup<T> {
    fn peek(input: &ParseBuffer) -> bool {
        try_peek_group::<T>(input, "{")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}
impl Token for Literal {}
impl Parse for Literal {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let mut s: &str = input.as_ref();

        // If the literal starts with a quote, it's a string.
        if s.starts_with('"') {
            let end = s[1..].find('"');
            if let Some(i) = end {
                let buf = s[1..i + 1]
                    .replace("\\\"", "\"")
                    .replace("\\\\", "\\");
                input.advance_n(i + 2);
                return Ok(Self::String(buf));
            } else {
                return Err(unexpected_seq!('\"', input));
            }
        }

        let mut buf = String::new();
        let mut is_float = false;
        let mut is_hex = false;

        // Process integer part.
        // Note that the form '.01' is not allowed. The '.' will be captured by
        // a leading Punct token.
        buf.extend(s.chars().take_while(|c| {
            if *c == '.' {
                is_float = true;
                true
            } else {
                c.is_digit(10)
            }
        }));
        s = &s[buf.len()..];

        if !buf.is_empty() {
            match s.chars().next() {
                // Process scientific notation.
                Some('e') | Some('E') => {
                    is_float = true;
                    buf.push('e');
                    s = &s[1..];

                    // Process exponent sign.
                    if let Some(c) = s.chars().next() {
                        if c == '+' || c == '-' {
                            buf.push(c);
                            s = &s[1..];
                        }
                    }
        
                    // Process exponent part.
                    buf.extend(s.chars().take_while(|c| c.is_digit(10)));
                }
                // Hexadecimal integer.
                Some('x') | Some('X') if !is_float => {
                    is_hex = true;
                    buf.push('x');
                    s = &s[1..];
                    buf.extend(s.chars().take_while(|c| c.is_digit(16)));
                }
                _ => {}
            }
        }

        if buf.is_empty() {
            Err(unexpected_seq!("literal", input))
        } else if is_float {
            let literal = buf.parse::<f64>()?;
            input.advance_n(buf.len());
            Ok(Self::Float(literal))
        } else {
            let literal = if is_hex {
                i64::from_str_radix(&buf[2..], 16)?
            } else {
                i64::from_str_radix(&buf, 10)?
            };
            input.advance_n(buf.len());
            Ok(Self::Int(literal))
        }
    }
}
impl Peek for Literal {
    fn peek(input: &ParseBuffer) -> bool {
        let s: &str = input.as_ref();
        if let Some(c) = s.chars().next() {
            c.is_digit(10) || c == '"'
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_punct() {
        let mut input = ParseBuffer::from(",");
        let _ = input.parse::<Token![,]>().unwrap();
        assert!(input.is_empty());
    }

    #[test]
    fn test_parse_ident() {
        let mut input = ParseBuffer::from("foo");
        let token = input.parse::<Ident>().unwrap();
        assert!(input.is_empty());
        assert_eq!(token.name, "foo");
    }

    #[test]
    fn test_parse_literal() {
        // String.
        {
            let mut input = ParseBuffer::from("\"foo\"");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal::String("foo".to_string()));
        }

        // Interger.
        {
            let mut input = ParseBuffer::from("123");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal::Int(123));
        }

        // Real number.
        {
            let mut input = ParseBuffer::from("123.456");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal::Float(123.456));
        }

        // Scientific notation.
        {
            let mut input = ParseBuffer::from("123.456e-7");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal::Float(123.456e-7));
        }
        {
            let mut input = ParseBuffer::from("123.456E+7");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal::Float(123.456E+7));
        }

        // Hexadecimal integer.
        {
            let mut input = ParseBuffer::from("0x123");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal::Int(0x123));
        }
    }
}
