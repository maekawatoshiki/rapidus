use ecow::EcoString;
use rapidus_ast::span::Span;

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
            name,
            text: text.into(),
        }
    }

    /// Returns an error report string at the span.
    pub fn error_message_at(&self, span: Span, message: impl Into<String>) -> String {
        let (line, row, column) = self.line_at(span);
        let mut s = format!("{}:{}:{}: {}", "error", row, column, message.into());
        s.push('\n');
        s.push_str(line);
        s.push('\n');
        for _ in 0..column - 1 {
            s.push(' ');
        }
        s.push('^');
        s
    }

    /// Returns a line including the span and its row and column.
    fn line_at(&self, span: Span) -> (&str, usize, usize) {
        let mut chars = 0;
        for (line_num, line) in self.text.lines().enumerate() {
            if chars <= span.start() && span.end() <= chars + line.len() + 1 {
                return (line, line_num + 1, span.start() - chars + 1);
            }
            chars += line.len() + '\n'.len_utf8();
        }
        panic!("Invalid span: {:?}", span);
    }
}
