use anyhow::{ bail, Result };
use super::{token::*, path::Path, punctuated::Punctuated, block::{Block, BlockHeader}, pat::Pat};
use crate::{Token, compiler::common::span::Span};
use super::{ Parse, ParseBuffer };


#[derive(Debug, Clone)]
pub struct ExprLiteral {
    pub literal: Literal,
    pub span: Span,
}
impl Parse for ExprLiteral {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let literal = input.parse::<Literal>()?;
        let span = literal.span();

        let out = Self {
            literal,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprPath {
    pub path: Path,
    pub span: Span,
}
impl Parse for ExprPath {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let path = input.parse::<Path>()?;
        let span = path.span;

        let out = Self {
            path,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprEmit {
    pub tilde_token: Token![~],
    pub opcode: Box<Pat>,
    pub operands: Option<Box<ParenGroup<Punctuated<Expr, Token![,]>>>>,
    pub result_type: Option<(Token![->], Box<Pat>)>,
    pub span: Span,
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

        let span = Span::join([
            tilde_token.span(),
            opcode.span(),
            operands.span(),
            result_type.span(),
        ]);

        let out = Self {
            tilde_token,
            opcode: Box::new(opcode),
            operands,
            result_type,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub param_name: Ident,
    pub colon_token: Token![:],
    pub value: Expr,
    pub span: Span,
}
impl Parse for Argument {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let param_name = input.parse::<Ident>()?;
        let colon_token = input.parse::<Token![:]>()?;
        let value = input.parse::<Expr>()?;

        let span = Span::join([
            param_name.span(),
            colon_token.span(),
            value.span(),
        ]);

        Ok(Self {
            param_name,
            colon_token,
            value,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ArgumentList {
    pub args: Box<Punctuated<Argument, Token![,]>>,
    pub span: Span,
}
impl Parse for ArgumentList {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let args = input.parse::<Punctuated<Argument, Token![,]>>()?;
        let span = args.span();

        return Ok(Self {
            args: Box::new(args),
            span,
        });
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprBlock {
    pub block_header: Option<BlockHeader>,
    pub block: BraceGroup<Block>,
    pub span: Span,
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

        let span = Span::join([
            block_header.span(),
            block.span(),
        ]);

        let out = Self {
            block_header,
            block,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub expr: Box<Expr>,
    pub args: ParenGroup<ArgumentList>,
    pub span: Span,
}
impl Parse for ExprCall {
    fn parse(_: &mut ParseBuffer) -> Result<Self> {
        panic!("use Expr::parse instead");
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprAssign {
    pub ident: Ident,
    pub eq_token: Token![=],
    pub expr: Box<Expr>,
    pub span: Span,
}
impl Parse for ExprAssign {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;

        let span = Span::join([
            ident.span(),
            eq_token.span(),
            expr.span(),
        ]);

        let out = Self {
            ident,
            eq_token,
            expr: Box::new(expr),
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
    Rem(Token![%]),
    Eq(Token![==]),
    Ne(Token![!=]),
    Lt(Token![<]),
    Le(Token![<=]),
    Gt(Token![>]),
    Ge(Token![>=]),
    LogicalAnd(Token![&&]),
    LogicalOr(Token![||]),
}
impl Parse for BinaryOp {
    fn parse(_: &mut ParseBuffer) -> Result<Self> {
        panic!("use ExprBinary::parse instead");
    }

    fn span(&self) -> Span {
        match self {
            Self::Add(t) => t.span(),
            Self::Sub(t) => t.span(),
            Self::Mul(t) => t.span(),
            Self::Div(t) => t.span(),
            Self::Rem(t) => t.span(),
            Self::Eq(t) => t.span(),
            Self::Ne(t) => t.span(),
            Self::Lt(t) => t.span(),
            Self::Le(t) => t.span(),
            Self::Gt(t) => t.span(),
            Self::Ge(t) => t.span(),
            Self::LogicalAnd(t) => t.span(),
            Self::LogicalOr(t) => t.span(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExprBinary {
    binary_op: BinaryOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    span: Span,
}
impl Parse for ExprBinary {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let lhs = input.parse::<Expr>()?;
        let binary_op = if input.peek::<Token![+]>() {
            BinaryOp::Add(input.parse::<Token![+]>()?)
        } else if input.peek::<Token![-]>() {
            BinaryOp::Sub(input.parse::<Token![-]>()?)
        } else if input.peek::<Token![*]>() {
            BinaryOp::Mul(input.parse::<Token![*]>()?)
        } else if input.peek::<Token![/]>() {
            BinaryOp::Div(input.parse::<Token![/]>()?)
        } else if input.peek::<Token![%]>() {
            BinaryOp::Rem(input.parse::<Token![%]>()?)
        } else if input.peek::<Token![==]>() {
            BinaryOp::Eq(input.parse::<Token![==]>()?)
        } else if input.peek::<Token![!=]>() {
            BinaryOp::Ne(input.parse::<Token![!=]>()?)
        } else if input.peek::<Token![<]>() {
            BinaryOp::Lt(input.parse::<Token![<]>()?)
        } else if input.peek::<Token![<=]>() {
            BinaryOp::Le(input.parse::<Token![<=]>()?)
        } else if input.peek::<Token![>]>() {
            BinaryOp::Gt(input.parse::<Token![>]>()?)
        } else if input.peek::<Token![>=]>() {
            BinaryOp::Ge(input.parse::<Token![>=]>()?)
        } else if input.peek::<Token![&&]>() {
            BinaryOp::LogicalAnd(input.parse::<Token![&&]>()?)
        } else if input.peek::<Token![||]>() {
            BinaryOp::LogicalOr(input.parse::<Token![||]>()?)
        } else {
            bail!("expected binary operator ({})", input.surrounding());
        };
        let rhs = input.parse::<Expr>()?;

        let span = Span::join([
            binary_op.span(),
            lhs.span(),
            rhs.span(),
        ]);

        let out = Self {
            binary_op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprIfThenElse {
    pub if_token: Token![if],
    pub condition: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Option<(Token![else], Box<Expr>)>,
    pub span: Span,
}
impl Parse for ExprIfThenElse {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let if_token = input.parse::<Token![if]>()?;
        let condition = input.parse::<Expr>()?;
        let then_branch = input.parse::<ExprBlock>()?;
        let then_branch = Expr::Block(then_branch);
        let else_branch = if input.peek::<Token![else]>() {
            let else_token = input.parse::<Token![else]>()?;
            let else_branch = input.parse::<ExprBlock>()?;
            let else_branch = Expr::Block(else_branch);
            Some((else_token, Box::new(else_branch)))
        } else {
            None
        };

        let span = Span::join([
            if_token.span(),
            condition.span(),
            then_branch.span(),
            else_branch.span(),
        ]);

        let out = Self {
            if_token,
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprWhile {
    pub while_token: Token![while],
    pub condition: Box<Expr>,
    pub body: Box<Expr>,
    pub span: Span,
}
impl Parse for ExprWhile {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let while_token = input.parse::<Token![while]>()?;
        let condition = input.parse::<Expr>()?;
        let body = input.parse::<ExprBlock>()?;
        let body = Expr::Block(body);

        let span = Span::join([
            while_token.span(),
            condition.span(),
            body.span(),
        ]);

        let out = Self {
            while_token,
            condition: Box::new(condition),
            body: Box::new(body),
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(ExprLiteral),
    Path(ExprPath),
    Emit(ExprEmit),
    Block(ExprBlock),
    Call(ExprCall),
    Assign(ExprAssign),
    IfThenElse(ExprIfThenElse),
    While(ExprWhile),
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
                let span = Span::join([
                    block.span(),
                    input.span(),
                ]);
                let call = ExprCall {
                    expr: Box::new(Self::Block(block)),
                    args: input.parse::<ParenGroup<ArgumentList>>()?,
                    span,
                };
                return Ok(Self::Call(call));
            } else {
                return Ok(Self::Block(block));
            }
        }

        if input.peek::<Token![if]>() {
            let if_then_else = input.parse::<ExprIfThenElse>()?;
            return Ok(Self::IfThenElse(if_then_else));
        }

        if input.peek::<Token![while]>() {
            let while_ = input.parse::<ExprWhile>()?;
            return Ok(Self::While(while_));
        }

        if input.peek::<Literal>() {
            let literal = input.parse::<ExprLiteral>()?;
            return Ok(Self::Literal(literal));
        } else if input.peek::<Ident>() {
            let path = input.parse::<ExprPath>()?;
            if input.peek::<ParenGroup<()>>() {
                let span = Span::join([
                    path.span(),
                    input.span(),
                ]);
                let call = ExprCall {
                    expr: Box::new(Self::Path(path)),
                    args: input.parse::<ParenGroup<ArgumentList>>()?,
                    span,
                };
                return Ok(Self::Call(call));
            } else {
                return Ok(Self::Path(path));
            }
        }

        bail!("expected literal or ident ({})", input.surrounding());
    }

    fn span(&self) -> Span {
        match self {
            Self::Literal(literal) => literal.span(),
            Self::Path(path) => path.span(),
            Self::Emit(emit) => emit.span(),
            Self::Block(block) => block.span(),
            Self::Call(call) => call.span(),
            Self::Assign(assign) => assign.span(),
            Self::IfThenElse(if_then_else) => if_then_else.span(),
            Self::While(while_) => while_.span(),
        }
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

    #[test]
    fn test_if_then_else() {
        let code = "if 1 { 2 } else { 3 }";
        let mut input = ParseBuffer::from(code);
        let expr = input.parse::<ExprIfThenElse>().unwrap();
        println!("{:#?}", expr);
        assert!(input.is_empty());
    }
}
