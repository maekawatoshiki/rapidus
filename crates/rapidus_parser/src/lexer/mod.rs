use crate::source::Source;

/// Lexical analyzer.
pub struct Lexer<'a> {
    #[allow(dead_code)]
    /// Input
    input: Input<'a>,
}

pub struct Input<'a> {
    /// Source text
    source: &'a str,

    /// Start position in `source`
    start: usize,

    /// End position in `source`
    end: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer.
    pub fn new(input: Input<'a>) -> Self {
        Lexer { input }
    }
}

impl<'a> Input<'a> {
    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

impl<'a> From<&'a Source> for Input<'a> {
    /// ```rust
    /// use rapidus_parser::source::{Source, SourceName};
    /// use rapidus_parser::lexer::Input;
    ///
    /// let _ = Input::from(&Source::new(
    ///     SourceName::FileName("test.js".into()),
    ///     "var x = 1;",
    /// ));
    /// ```
    fn from(source: &'a Source) -> Self {
        Input {
            source: source.text.as_str(),
            start: 0,
            end: source.text.len(),
        }
    }
}
