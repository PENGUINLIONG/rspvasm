use anyhow::{ bail, Result };
use super::{token::*, path::Path, punctuated::Punctuated, block::{Block, BlockHeader}, pat::Pat};
use crate::Token;
use super::{ Parse, ParseBuffer };


#[derive(Debug, Clone)]
pub struct ExprLiteral {
    pub literal: Literal,
}
impl Parse for ExprLiteral {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let literal = input.parse::<Literal>()?;
        Ok(Self { literal })
    }
}

#[derive(Debug, Clone)]
pub struct ExprPath {
    pub path: Path,
}
impl Parse for ExprPath {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let path = input.parse::<Path>()?;
        Ok(Self { path })
    }
}


#[derive(Debug, Clone)]
pub struct ExprEmit {
    pub tilde_token: Token![~],
    pub opcode: Box<Pat>,
    pub operands: Option<Box<ParenGroup<Punctuated<Expr, Token![,]>>>>,
    pub result_type: Option<(Token![->], Box<Pat>)>,
}
impl Parse for ExprEmit {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let tilde_token = input.parse::<Token![~]>()?;
        let opcode = input.parse::<Pat>()?;
        let operands = if input.peek::<ParenGroup<()>>() {
            let group = input.parse::<ParenGroup<Punctuated<Expr, Token![,]>>>()?;
            Some(Box::new(group))
        } else {
            None
        };
        let result_type = if input.peek::<Token![->]>() {
            input.parse::<Option<(Token![->], Box<Pat>)>>()?
        } else {
            None
        };

        let out = Self {
            tilde_token,
            opcode: Box::new(opcode),
            operands,
            result_type,
        };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub param_name: Ident,
    pub colon_token: Token![:],
    pub value: Expr,
}
impl Parse for Argument {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let param_name = input.parse::<Ident>()?;
        let colon_token = input.parse::<Token![:]>()?;
        let value = input.parse::<Expr>()?;
        Ok(Self {
            param_name,
            colon_token,
            value,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ArgumentList {
    pub args: Box<Punctuated<Argument, Token![,]>>,
}
impl Parse for ArgumentList {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let args = input.parse::<Punctuated<Argument, Token![,]>>()?;

        return Ok(Self {
            args: Box::new(args)
        });
    }
}

#[derive(Debug, Clone)]
pub struct ExprBlock {
    pub block_header: Option<BlockHeader>,
    pub block: BraceGroup<Block>,
}
impl Parse for ExprBlock {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let block_header = if input.peek::<Token![|]>() {
            let block_header = input.parse::<BlockHeader>()?;
            Some(block_header)
        } else {
            None
        };
        let block = input.parse::<BraceGroup<Block>>()?;
        Ok(Self { block_header, block })
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub expr: Box<Expr>,
    pub args: ParenGroup<ArgumentList>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(ExprLiteral),
    Path(ExprPath),
    Emit(ExprEmit),
    Block(ExprBlock),
    Call(ExprCall),
}
impl Parse for Expr {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        if input.peek::<Token![~]>() {
            let emit = input.parse::<ExprEmit>()?;
            return Ok(Self::Emit(emit));
        }
        if input.peek::<Token![|]>() || input.peek::<BraceGroup<()>>() {
            let block = input.parse::<ExprBlock>()?;
            if input.peek::<ParenGroup<()>>() {
                let call = ExprCall {
                    expr: Box::new(Self::Block(block)),
                    args: input.parse::<ParenGroup<ArgumentList>>()?,
                };
                return Ok(Self::Call(call));
            } else {
                return Ok(Self::Block(block));
            }
        }

        if input.peek::<Literal>() {
            let literal = input.parse::<ExprLiteral>()?;
            return Ok(Self::Literal(literal));
        } else if input.peek::<Ident>() {
            let path = input.parse::<ExprPath>()?;
            if input.peek::<ParenGroup<()>>() {
                let call = ExprCall {
                    expr: Box::new(Self::Path(path)),
                    args: input.parse::<ParenGroup<ArgumentList>>()?,
                };
                return Ok(Self::Call(call));
            } else {
                return Ok(Self::Path(path));
            }
        }

        bail!("expected literal or ident ({})", input.surrounding());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr_emit() {
        let code = "~0(1, 2, 3)";
        let mut input = ParseBuffer::from(code);
        let expr = input.parse::<ExprEmit>().unwrap();
        println!("{:#?}", expr);
        assert!(input.is_empty());
    }

    #[test]
    fn test_parse_expr_emit_with_result_no_type() {
        let code = "~0(1, 2, 3) -> id";
        let mut input = ParseBuffer::from(code);
        let expr = input.parse::<ExprEmit>().unwrap();
        println!("{:#?}", expr);
        assert!(input.is_empty());
    }

    #[test]
    fn test_parse_expr_emit_with_block() {
        let code = "{ ~0(1, 2, 3) }";
        let mut input = ParseBuffer::from(code);
        let expr = input.parse::<ExprBlock>().unwrap();
        println!("{:#?}", expr);
        assert!(input.is_empty());
    }

    #[test]
    fn test_parse_expr_emit_with_block_with_params() {
        let code = "|a, b| { ~0(1, 2, 3) }";
        let mut input = ParseBuffer::from(code);
        let expr = input.parse::<ExprBlock>().unwrap();
        println!("{:#?}", expr);
        assert!(input.is_empty());
    }
}
