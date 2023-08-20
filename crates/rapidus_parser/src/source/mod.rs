use ecow::EcoString;

/// Represents a source.
pub struct Source {
    pub name: SourceName,
    pub text: EcoString,
}

/// Represents a source name.
pub enum SourceName {
    /// File name
    FileName(String),

    /// Anything other than file name
    Custom(String),
}

impl Source {
    /// Creates a new source.
    ///
    /// ```rust
    /// use rapidus_parser::source::{Source, SourceName};
    ///
    /// let _ = Source::new(SourceName::FileName("hello.js".into()), "console.log('hello world!');");
    /// let _ = Source::new(SourceName::Custom("hello".into()), "console.log('hello world!');");
    /// ```
    pub fn new(name: SourceName, text: impl Into<EcoString>) -> Self {
        Self {
            name: name.into(),
            text: text.into(),
        }
    }
}
