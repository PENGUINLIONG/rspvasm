use super::{Parse, ParseBuffer, Peek};
use crate::compiler::common::span::Span;
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

    fn span(&self) -> Span {
        match self {
            TokenTree::Ident(ident) => ident.span,
            TokenTree::Punct(punct) => punct.span,
            TokenTree::Literal(literal) => literal.span,
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
            #[derive(Debug, Clone, PartialEq, Eq)]
            pub struct $ty {
                pub span: Span
            }
            impl $ty {
                pub fn with_span(span: Span) -> Self {
                    Self { span }
                }
            }
            impl Token for $ty {}
            impl Parse for $ty {
                fn parse(input: &mut ParseBuffer) -> Result<Self> {
                    let lo = input.pos;
                    let s: &str = input.as_ref();
                    if s.starts_with($name) {
                        input.advance_n($name.len());
                        let hi = input.pos;
                        Ok(Self::with_span(Span { lo, hi }))
                    } else {
                        Err(unexpected_seq!($name, input))
                    }
                }

                fn span(&self) -> Span {
                    self.span
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
    "ref" => Ref
    "type" => Type
    "const" => Const
    "struct" => Struct
    "impl" => Impl
    "layout" => Layout
    "if" => If
    "else" => Else
    "while" => While
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
    "#" => Hash
}

#[macro_export]
macro_rules! Token {
    [let] => { $crate::compiler::syn::token::Let };
    [ref] => { $crate::compiler::syn::token::Ref };
    [type] => { $crate::compiler::syn::token::Type };
    [const] => { $crate::compiler::syn::token::Const };
    [struct] => { $crate::compiler::syn::token::Struct };
    [impl] => { $crate::compiler::syn::token::Impl };
    [layout] => { $crate::compiler::syn::token::Layout };
    [if] => { $crate::compiler::syn::token::If };
    [else] => { $crate::compiler::syn::token::Else };
    [while] => { $crate::compiler::syn::token::While };
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
    [#] => { $crate::compiler::syn::token::Hash };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}
impl Token for Ident {}
impl Parse for Ident {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lo = input.pos;
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
            let hi = input.pos;

            let out = Self {
                name: buf,
                span: Span { lo, hi },
            };
            Ok(out)
        }
    }

    fn span(&self) -> Span {
        self.span
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Spacing {
    Alone,
    Joint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punct {
    pub ch: char,
    pub spacing: Spacing,
    pub span: Span,
}
impl Token for Punct {}
impl Parse for Punct {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lo = input.pos;
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
                let hi = input.pos;

                let out = Self {
                    ch: c,
                    spacing,
                    span: Span { lo, hi },
                };
                Ok(out)
            }
            _ => {
                return Err(unexpected_seq!("punctuation", input));
            }
        }
    }

    fn span(&self) -> Span {
        self.span
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
pub struct ParenGroup<T> {
    pub inner: T,
    pub span: Span,
}
impl<T: Parse> Parse for ParenGroup<T> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lo = input.pos;
        let inner = try_parse_group::<T>(input, "(", ")")?;
        let hi = input.pos;
        Ok(Self { inner, span: Span { lo, hi } })
    }

    fn span(&self) -> Span {
        self.span
    }
}
impl<T: Peek> Peek for ParenGroup<T> {
    fn peek(input: &ParseBuffer) -> bool {
        try_peek_group::<T>(input, "(")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BracketGroup<T> {
    pub inner: T,
    pub span: Span,
}
impl<T: Parse> Parse for BracketGroup<T> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lo = input.pos;
        let inner = try_parse_group::<T>(input, "[", "]")?;
        let hi = input.pos;
        Ok(Self { inner, span: Span { lo, hi } })
    }

    fn span(&self) -> Span {
        self.span
    }
}
impl<T: Peek> Peek for BracketGroup<T> {
    fn peek(input: &ParseBuffer) -> bool {
        try_peek_group::<T>(input, "[")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BraceGroup<T> {
    pub inner: T,
    pub span: Span,
}
impl<T: Parse> Parse for BraceGroup<T> {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lo = input.pos;
        let inner = try_parse_group::<T>(input, "{", "}")?;
        let hi = input.pos;
        Ok(Self { inner, span: Span { lo, hi } })
    }

    fn span(&self) -> Span {
        self.span
    }
}
impl<T: Peek> Peek for BraceGroup<T> {
    fn peek(input: &ParseBuffer) -> bool {
        try_peek_group::<T>(input, "{")
    }
}

#[derive(Debug, Clone)]
pub enum Lit {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}
impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => false,
        }
    }
}
impl std::cmp::Eq for Lit {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub lit: Lit,
    pub span: Span,
}
impl Token for Literal {}
impl Parse for Literal {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lo = input.pos;
        let mut s: &str = input.as_ref();

        if s.starts_with("true") {
            input.advance_n("true".len());

            let hi = input.pos;
            let out = Literal {
                lit: Lit::Bool(true),
                span: Span { lo, hi },
            };
            return Ok(out);
        } else if s.starts_with("false") {
            input.advance_n("false".len());

            let hi = input.pos;
            let out = Literal {
                lit: Lit::Bool(false),
                span: Span { lo, hi },
            };
            return Ok(out);
        }

        // If the literal starts with a quote, it's a string.
        if s.starts_with('"') {
            let end = s[1..].find('"');
            if let Some(i) = end {
                let buf = s[1..i + 1]
                    .replace("\\\"", "\"")
                    .replace("\\\\", "\\");
                input.advance_n(i + 2);

                let hi = input.pos;
                let out = Self {
                    lit: Lit::String(buf),
                    span: Span { lo, hi },
                };
                return Ok(out);
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
            let hi = input.pos;

            let out = Self {
                lit: Lit::Float(literal),
                span: Span { lo, hi },
            };
            Ok(out)
        } else {
            let literal = if is_hex {
                i64::from_str_radix(&buf[2..], 16)?
            } else {
                i64::from_str_radix(&buf, 10)?
            };
            input.advance_n(buf.len());
            let hi = input.pos;

            let out = Self {
                lit: Lit::Int(literal),
                span: Span { lo, hi },
            };
            Ok(out)
        }
    }

    fn span(&self) -> Span {
        self.span
    }
}
impl Peek for Literal {
    fn peek(input: &ParseBuffer) -> bool {
        let s: &str = input.as_ref();
        if let Some(c) = s.chars().next() {
            c.is_digit(10) || c == '"' || s.starts_with("true") || s.starts_with("false")
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
        // Bool.
        {
            let mut input = ParseBuffer::from("true");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Bool(true),
                span: Span { lo: 0, hi: 4 },
            });
        }
        {
            let mut input = ParseBuffer::from("false");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Bool(false),
                span: Span { lo: 0, hi: 5 },
            });
        }

        // String.
        {
            let mut input = ParseBuffer::from("\"foo\"");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::String("foo".to_string()),
                span: Span { lo: 0, hi: 5 },
            });
        }

        // Interger.
        {
            let mut input = ParseBuffer::from("123");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Int(123),
                span: Span { lo: 0, hi: 3 },
            });
        }

        // Real number.
        {
            let mut input = ParseBuffer::from("123.456");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Float(123.456),
                span: Span { lo: 0, hi: 7 },
            });
        }

        // Scientific notation.
        {
            let mut input = ParseBuffer::from("123.456e-7");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Float(123.456e-7),
                span: Span { lo: 0, hi: 10 },
            });
        }
        {
            let mut input = ParseBuffer::from("123.456E+7");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Float(123.456E+7),
                span: Span { lo: 0, hi: 10 },
            });
        }

        // Hexadecimal integer.
        {
            let mut input = ParseBuffer::from("0x123");
            let token = input.parse::<Literal>().unwrap();
            assert!(input.is_empty());
            assert_eq!(token, Literal {
                lit: Lit::Int(0x123),
                span: Span { lo: 0, hi: 5 },
            });
        }
    }

    #[test]
    fn test_parse_group() {
        let mut input = ParseBuffer::from("(foo)");
        let token = input.parse::<ParenGroup<Ident>>().unwrap();
        assert!(input.is_empty());
        assert_eq!(token.inner, Ident {
            name: "foo".to_string(),
            span: Span { lo: 1, hi: 4 },
        });
    }
}
