use super::{expr::Expr, meta::MetaList, pat::Pat, punctuated::Punctuated, token::*};
use super::{Parse, ParseBuffer};
use crate::{compiler::common::span::Span, Token};
use anyhow::Result;

#[derive(Debug, Clone)]
pub struct StmtLocal {
    pub meta_list: MetaList,
    pub let_token: Token![let],
    pub mut_token: Option<Token![mut]>,
    pub name: Ident,
    pub eq_token: Token![=],
    pub expr: Box<Expr>,
    pub semi_token: Token![;],
    pub span: Span,
}
impl Parse for StmtLocal {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let let_token = input.parse::<Token![let]>()?;
        let mut_token = input
            .peek::<Token![mut]>()
            .then(|| input.parse::<Token![mut]>())
            .transpose()?;
        let name = input.parse::<Ident>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let expr = input.parse::<Expr>()?;
        let semi_token = input.parse::<Token![;]>()?;

        let span = Span::join([
            let_token.span(),
            mut_token.span(),
            name.span(),
            eq_token.span(),
            expr.span(),
            semi_token.span(),
        ]);

        let out = Self {
            meta_list: MetaList::default(),
            let_token,
            mut_token,
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

        let span = Span::join([expr.span(), semi_token.span()]);

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

        let span = Span::join([ident.span(), eq_token.span(), expr.span()]);

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
    pub brace_group: BraceGroup,
    pub variants: Punctuated<Variant, Token![,]>,
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
        let brace_group = input.parse::<BraceGroup>()?;
        let variants = brace_group
            .inner
            .clone()
            .parse::<Punctuated<Variant, Token![,]>>()?;

        let span = Span::join([
            const_token.span(),
            name.span(),
            brace_group.span(),
            variants.span(),
        ]);

        let out = Self {
            meta_list: MetaList::default(),
            const_token,
            name,
            brace_group,
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

        let span = Span::join([pat.span(), eq_token.span(), expr.span()]);

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
    pub brace_group: BraceGroup,
    pub entries: Punctuated<LayoutEntry, Token![,]>,
    pub span: Span,
}
impl Parse for StmtLayout {
    fn parse(input: &mut ParseBuffer) -> Result<Self> {
        let layout_token = input.parse::<Token![layout]>()?;
        let brace_group = input.parse::<BraceGroup>()?;
        let entries = brace_group
            .inner
            .clone()
            .parse::<Punctuated<LayoutEntry, Token![,]>>()?;

        let span = Span::join([layout_token.span(), brace_group.span(), entries.span()]);

        let out = Self {
            meta_list: MetaList::default(),
            brace_group,
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
    Local(StmtLocal),
    Expr(StmtExpr),
    Const(StmtConst),
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
