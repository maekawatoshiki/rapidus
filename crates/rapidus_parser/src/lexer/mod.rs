use std::str::CharIndices;

use ecow::EcoString;
use thiserror::Error;

use crate::{source::Source, token::Token};

/// Lexical analyzer.
pub struct Lexer<'a> {
    #[allow(dead_code)]
    /// Input
    input: Input<'a>,
}

pub struct Input<'a> {
    /// Source text
    source: &'a EcoString,

    /// Iterator used to read each character and its position
    chars: CharIndices<'a>,

    /// Current position in `chars`
    pos_in_chars: usize,

    /// Start position in `source`
    start: usize,

    /// End position in `source`
    end: usize,
}

#[derive(Debug, Error)]
pub enum LexerError {}

impl<'a> Lexer<'a> {
    /// Creates a new lexer.
    pub fn new(input: Input<'a>) -> Self {
        Lexer { input }
    }

    /// Reads a token from `input`.
    pub fn read_token(&mut self) -> Result<Option<Token>, LexerError> {
        if self.input.is_empty() {
            return Ok(None);
        }

        match self.input.peek().unwrap() {
            // TODO: https://tc39.es/ecma262/#prod-IdentifierStart
            'a'..='z' | 'A'..='Z' | '_' => self.read_ident().map(Some),
            _ => todo!(),
        }
    }

    fn read_ident(&mut self) -> Result<Token, LexerError> {
        todo!()
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

    pub fn is_empty(&self) -> bool {
        self.pos_in_chars >= self.end
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().peekable().peek().map(|(_, c)| *c)
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
            source: &source.text,
            chars: source.text[0..source.text.len()].char_indices(),
            pos_in_chars: 0,
            start: 0,
            end: source.text.len(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Source, SourceName};

    #[test]
    fn lex_empty() {
        let source = Source::new(SourceName::FileName("test.js".into()), "");
        let mut lexer = Lexer::new(Input::from(&source));
        assert_eq!(lexer.read_token().unwrap(), None);
    }

    #[test]
    #[should_panic]
    fn lex_hello() {
        let source = Source::new(SourceName::FileName("test.js".into()), "hello");
        let mut lexer = Lexer::new(Input::from(&source));
        assert_ne!(lexer.read_token().unwrap(), None);
    }
}
