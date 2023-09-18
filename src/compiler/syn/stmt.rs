use anyhow::Result;
use super::{ token::*, expr::Expr, punctuated::Punctuated, pat::Pat, meta::MetaList };
use crate::{Token, compiler::common::span::Span};
use super::{ Parse, ParseBuffer };

#[derive(Debug, Clone)]
pub struct StmtLocal {
    pub meta_list: MetaList,
    pub let_token: Token![let],
    pub name: Ident,
    pub eq_token: Token![=],
    pub expr: Box<Expr>,
    pub semi_token: Token![;],
    pub span: Span,
}
impl Parse for StmtLocal {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let let_token = input.parse::<Token![let]>()?;
        let name = input.parse::<Ident>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;
        let semi_token = input.parse::<Token![;]>()?;

        let span = Span::join([
            let_token.span(),
            name.span(),
            eq_token.span(),
            expr.span(),
            semi_token.span(),
        ]);

        let out = Self {
            meta_list: MetaList::default(),
            let_token,
            name,
            eq_token,
            expr: Box::new(expr),
            semi_token,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct StmtExpr {
    pub meta_list: MetaList,
    pub expr: Box<Expr>,
    pub semi_token: Option<Token![;]>,
    pub span: Span,
}
impl Parse for StmtExpr {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let expr = input.parse::<Expr>()?;
        let semi_token = if input.peek::<Token![;]>() {
            Some(input.parse::<Token![;]>()?)
        } else {
            None
        };

        let span = Span::join([
            expr.span(),
            semi_token.span(),
        ]);

        let out = Self {
            meta_list: MetaList::default(),
            expr: Box::new(expr),
            semi_token,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub ident: Ident,
    pub eq_token: Token![=],
    pub expr: Expr,
    pub span: Span,
}
impl Parse for Variant {
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
            expr,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct StmtConst {
    pub meta_list: MetaList,
    pub const_token: Token![const],
    pub name: Option<Ident>,
    pub variants: BraceGroup<Punctuated<Variant, Token![,]>>,
    pub span: Span,
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

        let span = Span::join([
            const_token.span(),
            name.span(),
            variants.span(),
        ]);

        let out = Self {
            meta_list: MetaList::default(),
            const_token,
            name,
            variants,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct LayoutEntry {
    pub pat: Pat,
    pub eq_token: Token![=],
    pub expr: Expr,
    pub span: Span,
}
impl Parse for LayoutEntry {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let pat = input.parse::<Pat>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;

        let span = Span::join([
            pat.span(),
            eq_token.span(),
            expr.span(),
        ]);

        let out = Self {
            pat,
            eq_token,
            expr,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct StmtLayout {
    pub meta_list: MetaList,
    pub layout_token: Token![layout],
    pub entries: BraceGroup<Punctuated<LayoutEntry, Token![,]>>,
    pub span: Span,
}
impl Parse for StmtLayout {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let layout_token = input.parse::<Token![layout]>()?;
        let entries = input.parse::<BraceGroup<Punctuated<LayoutEntry, Token![,]>>>()?;

        let span = Span::join([
            layout_token.span(),
            entries.span(),
        ]);

        let out = Self {
            meta_list: MetaList::default(),
            layout_token,
            entries,
            span,
        };
        Ok(out)
    }

    fn span(&self) -> Span {
        self.span
    }
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
        let meta_list = if input.peek::<Token![#]>() {
            input.parse::<MetaList>()?
        } else {
            MetaList::default()
        };

        if input.peek::<Token![let]>() {
            let mut local = input.parse::<StmtLocal>()?;
            local.meta_list = meta_list;
            Ok(Self::Local(local))
        } else if input.peek::<Token![const]>() {
            let mut const_ = input.parse::<StmtConst>()?;
            const_.meta_list = meta_list;
            Ok(Self::Const(const_))
        } else if input.peek::<Token![layout]>() {
            let mut layout = input.parse::<StmtLayout>()?;
            layout.meta_list = meta_list;
            Ok(Self::Layout(layout))
        } else {
            let mut expr = input.parse::<StmtExpr>()?;
            expr.meta_list = meta_list;
            Ok(Self::Expr(expr))
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::Local(local) => local.span(),
            Self::Expr(expr) => expr.span(),
            Self::Const(const_) => const_.span(),
            Self::Layout(layout) => layout.span(),
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
