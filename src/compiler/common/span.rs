use std::cell::RefCell;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}
impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub fn call_site() -> Self {
        Self { lo: 0, hi: 0 }
    }

    pub fn join<I: IntoIterator<Item = Span>>(spans: I) -> Self {
        let mut builder = SpanBuilder::new();
        for span in spans {
            builder.push(span);
        }
        builder.into()
    }
}

pub struct SpanBuilder {
    inner: Option<Span>,
}
impl SpanBuilder {
    pub fn new() -> Self {
        Self { inner: None }
    }

    pub fn push(&mut self, span: Span) {
        if let Some(inner) = self.inner.as_mut() {
            let lo = inner.lo.min(span.lo);
            let hi = inner.hi.max(span.hi);
            *inner = Span::new(lo, hi);
        } else {
            self.inner = Some(span);
        }
    }
}
impl Into<Span> for SpanBuilder {
    fn into(self) -> Span {
        self.inner.unwrap_or(Span::call_site())
    }
}

#[derive(Debug, Clone)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct FileInfo {
    pub source_text: String,
    pub span: Span,
    pub lines: Vec<usize>,
}

thread_local!(
    static FILE_INFOS: RefCell<Vec<FileInfo>> = RefCell::new(vec![
        FileInfo {
            source_text: String::from(""),
            span: Span { lo: 0, hi: 0 },
            lines: vec![0],
        }
    ]);
);
