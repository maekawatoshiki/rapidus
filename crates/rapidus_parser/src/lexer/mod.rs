use std::str::Chars;

use ecow::EcoString;
use thiserror::Error;

use crate::{
    source::Source,
    token::{
        ident::{Ident, ReservedWord},
        Token,
    },
};

/// Lexical analyzer.
pub struct Lexer<'a> {
    #[allow(dead_code)]
    /// Input
    input: Input<'a>,
}

pub struct Input<'a> {
    /// Source text
    source: &'a EcoString,

    /// Iterator used to read each character in `source`
    chars: Chars<'a>,

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

        match self.input.cur().unwrap() {
            // TODO: https://tc39.es/ecma262/#prod-IdentifierStart
            'a'..='z' | 'A'..='Z' | '_' => self.read_ident().map(Some),
            c if c.is_whitespace() => self.read_whitespace().map(Some),
            _ => todo!(),
        }
    }

    fn read_ident(&mut self) -> Result<Token, LexerError> {
        let s = self.input.take_while(char::is_ascii_alphanumeric);
        Ok(Token::Ident(
            ReservedWord::try_from(s.clone())
                .map(Ident::from)
                .unwrap_or_else(|_| Ident::Ident(s)),
        ))
    }

    fn read_whitespace(&mut self) -> Result<Token, LexerError> {
        let s = self.input.take_while(char::is_ascii_whitespace);
        Ok(Token::Whitespace(s))
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

    pub fn cur(&self) -> Option<char> {
        self.chars.clone().peekable().peek().copied()
    }

    pub fn take_while<F>(&mut self, mut pred: F) -> EcoString
    where
        F: FnMut(&char) -> bool,
    {
        let start_pos = self.pos_in_chars;
        let mut out = EcoString::new();
        for c in self.chars.as_str().chars().take_while(|c| pred(c)) {
            self.pos_in_chars += c.len_utf8();
            out.push(c);
        }
        self.chars = self.chars.as_str()[self.pos_in_chars - start_pos..].chars();
        out
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
            chars: source.text[0..source.text.len()].chars(),
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
    fn lex_hello() {
        let source = Source::new(SourceName::FileName("test.js".into()), "hello");
        let mut lexer = Lexer::new(Input::from(&source));
        insta::assert_debug_snapshot!(lexer.read_token().unwrap());
    }

    #[test]
    fn lex_hello123() {
        let source = Source::new(SourceName::FileName("test.js".into()), "hello123");
        let mut lexer = Lexer::new(Input::from(&source));
        insta::assert_debug_snapshot!(lexer.read_token().unwrap());
    }

    #[test]
    fn lex_reserved_if() {
        let source = Source::new(SourceName::FileName("test.js".into()), "if");
        let mut lexer = Lexer::new(Input::from(&source));
        insta::assert_debug_snapshot!(lexer.read_token().unwrap());
    }

    #[test]
    fn lex_reserved_words() {
        let source = Source::new(SourceName::FileName("test.js".into()), "if while    await");
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }
}
