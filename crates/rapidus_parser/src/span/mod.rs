/// Represents a span of something.
#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    inner: T,
    start: usize,
    end: usize,
}

impl<T> Spanned<T> {
    pub fn inner_ref(&self) -> &T {
        &self.inner
    }

    pub fn inner(self) -> T {
        self.inner
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}
