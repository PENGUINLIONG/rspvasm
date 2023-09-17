use anyhow::Result;
use super::{ token::*, expr::Expr, punctuated::Punctuated, pat::Pat };
use crate::Token;
use super::{ Parse, ParseBuffer };

#[derive(Debug, Clone)]
pub struct StmtLocal {
    pub let_token: Token![let],
    pub name: Ident,
    pub eq_token: Token![=],
    pub expr: Box<Expr>,
    pub semi_token: Token![;],
}
impl Parse for StmtLocal {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let let_token = input.parse::<Token![let]>()?;
        let name = input.parse::<Ident>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;
        let semi_token = input.parse::<Token![;]>()?;

        let out = Self {
            let_token,
            name,
            eq_token,
            expr: Box::new(expr),
            semi_token,
        };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct StmtExpr {
    pub expr: Box<Expr>,
    pub semi_token: Option<Token![;]>,
}
impl Parse for StmtExpr {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let expr = input.parse::<Expr>()?;
        let semi_token = if input.peek::<Token![;]>() {
            Some(input.parse::<Token![;]>()?)
        } else {
            None
        };

        let out = Self { expr: Box::new(expr), semi_token };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub ident: Ident,
    pub eq_token: Token![=],
    pub expr: Expr,
}
impl Parse for Variant {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;

        let out = Self { ident, eq_token, expr };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct StmtConst {
    pub const_token: Token![const],
    pub name: Option<Ident>,
    pub variants: BraceGroup<Punctuated<Variant, Token![,]>>,
}
impl Parse for StmtConst {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let const_token = input.parse::<Token![const]>()?;
        let name = if input.peek::<Ident>() {
            Some(input.parse::<Ident>()?)
        } else {
            None
        };
        let variants = input.parse::<BraceGroup<Punctuated<Variant, Token![,]>>>()?;

        let out = Self { const_token, name, variants };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct LayoutEntry {
    pub pat: Pat,
    pub eq_token: Token![=],
    pub expr: Expr,
}
impl Parse for LayoutEntry {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let pat = input.parse::<Pat>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;

        let out = Self { pat, eq_token, expr };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct StmtLayout {
    pub layout_token: Token![layout],
    pub entries: BraceGroup<Punctuated<LayoutEntry, Token![,]>>,
}
impl Parse for StmtLayout {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let layout_token = input.parse::<Token![layout]>()?;
        let entries = input.parse::<BraceGroup<Punctuated<LayoutEntry, Token![,]>>>()?;

        let out = Self { layout_token, entries };
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub param_name: Ident,
    pub colon_token: Token![:],
    pub arg_name: Ident,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Fundamental.
    Local(StmtLocal),
    /// Fundamental.
    Expr(StmtExpr),
    /// Fundamental.
    Const(StmtConst),
    /// Fundamental.
    Layout(StmtLayout),
}
impl Parse for Stmt {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        if input.peek::<Token![let]>() {
            let local = input.parse::<StmtLocal>()?;
            Ok(Self::Local(local))
        } else if input.peek::<Token![const]>() {
            let const_ = input.parse::<StmtConst>()?;
            Ok(Self::Const(const_))
        } else if input.peek::<Token![layout]>() {
            let layout = input.parse::<StmtLayout>()?;
            Ok(Self::Layout(layout))
        } else {
            let expr = input.parse::<StmtExpr>()?;
            Ok(Self::Expr(expr))
        }
    }
}

pub fn parse_stmts(input: &mut ParseBuffer) -> Result<Vec<Stmt>> {
    let mut out = Vec::new();

    loop {
        if input.is_empty() {
            break;
        }
        let stmt = input.parse::<Stmt>()?;
        out.push(stmt);
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_parse_stmt() {
        let code = r#"
        let _ = ~7(0);
        ~0()
        "#;
        let mut input = crate::compiler::syn::ParseBuffer::from(code);
        let stmts = super::parse_stmts(&mut input).unwrap();
        println!("{:#?}", stmts);
    }

    #[test]
    fn test_parse_stmt_with_return_type() {
        let code = "let _ = ~0(1, 2, 3) -> 1;";
        let mut input = crate::compiler::syn::ParseBuffer::from(code);
        let stmts = super::parse_stmts(&mut input).unwrap();
        println!("{:#?}", stmts);
        assert!(input.is_empty());
    }

    #[test]
    fn test_parse_const_stmt() {
        let code = r#"
        const Foo {
            Bar = 0,
            Baz = 1.0,
        }
        const {
            Zoo = 1e-3
        }
        "#;
        let mut input = crate::compiler::syn::ParseBuffer::from(code);
        let stmts = super::parse_stmts(&mut input).unwrap();
        println!("{:#?}", stmts);
    }

    #[test]
    fn test_parse_template() {
        let code = r#"
        let add = |a, b| {
            let c = ~a(b);
        };
        "#;
        let mut input = crate::compiler::syn::ParseBuffer::from(code);
        let stmts = super::parse_stmts(&mut input).unwrap();
        println!("{:#?}", stmts);
    }
}
