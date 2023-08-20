/// Represents a span of something.
#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
}

/// Represents a spanned value.
#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub Span);

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

impl<T> Spanned<T> {
    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn span(&self) -> &Span {
        &self.1
    }
}
