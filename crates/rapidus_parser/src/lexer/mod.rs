use std::str::Chars;

use ecow::EcoString;
use thiserror::Error;

use crate::{
    source::Source,
    token::{
        comment::Comment,
        ident::{Ident, ReservedWord},
        is_line_terminator, is_whitespace,
        op::{AssignOp, Op},
        Token,
    },
};

/// Lexical analyzer.
pub struct Lexer<'a> {
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
pub enum LexerError {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("TODO")]
    Todo,
}

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
            '/' if matches!(self.input.cur2(), Some(('/', '/')) | Some(('/', '*'))) => {
                self.read_comments().map(Some)
            }
            // TODO: https://tc39.es/ecma262/#prod-Punctuator
            '+' | '-' | '(' | ')' | '{' | '}' | '[' | ']' | '.' | ';' | ':' | ',' | '<' | '>'
            | '=' | '!' | '~' | '?' | '&' | '|' | '^' | '%' | '*' | '/' => {
                self.read_punctuator().map(Some)
            }
            '\n' | '\r' | '\u{2028}' | '\u{2029}' => self.read_line_terminators().map(Some),
            c if is_whitespace(c) => self.read_whitespaces().map(Some),
            c => Err(LexerError::UnexpectedCharacter(c)),
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

    fn read_punctuator(&mut self) -> Result<Token, LexerError> {
        let c = self.input.advance().unwrap();
        match c {
            '+' if self.input.skip('+') => Ok(Token::Op(Op::PlusPlus)),
            '+' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::Add)),
            '-' if self.input.skip('-') => Ok(Token::Op(Op::MinusMinus)),
            '-' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::Sub)),
            '+' => Ok(Token::Op(Op::Plus)),
            '-' => Ok(Token::Op(Op::Minus)),
            '*' if self.input.skips(['*', '=']) => Ok(Token::AssignOp(AssignOp::Exp)),
            '*' if self.input.skip('*') => Ok(Token::Op(Op::Exp)),
            '*' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::Mul)),
            '*' => Ok(Token::Op(Op::Asterisk)),
            '/' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::Div)),
            '/' => Ok(Token::Op(Op::Div)),
            '%' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::Mod)),
            '%' => Ok(Token::Op(Op::Mod)),
            '&' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::BitAnd)),
            '&' if self.input.skips(['&', '=']) => Ok(Token::AssignOp(AssignOp::And)),
            '&' if self.input.skip('&') => Ok(Token::Op(Op::And)),
            '|' if self.input.skips(['|', '=']) => Ok(Token::AssignOp(AssignOp::Or)),
            '|' if self.input.skip('|') => Ok(Token::Op(Op::Or)),
            '|' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::BitOr)),
            '&' => Ok(Token::Op(Op::BitAnd)),
            '|' => Ok(Token::Op(Op::BitOr)),
            '^' if self.input.skip('=') => Ok(Token::AssignOp(AssignOp::BitXor)),
            '^' => Ok(Token::Op(Op::BitXor)),
            '~' => Ok(Token::Op(Op::BitNot)),
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            '[' => Ok(Token::LBracket),
            ']' => Ok(Token::RBracket),
            '.' if self.input.skips(['.', '.']) => Ok(Token::Op(Op::Ellipsis)),
            '.' => Ok(Token::Op(Op::Dot)),
            ';' => Ok(Token::Op(Op::Semicolon)),
            ':' => Ok(Token::Op(Op::Colon)),
            ',' => Ok(Token::Op(Op::Comma)),
            '<' if self.input.skips(['<', '=']) => Ok(Token::AssignOp(AssignOp::LShift)),
            '>' if self.input.skips(['>', '=']) => Ok(Token::AssignOp(AssignOp::RShift)),
            '<' if self.input.skip('<') => Ok(Token::Op(Op::LShift)),
            '>' if self.input.skips(['>', '>', '=']) => Ok(Token::AssignOp(AssignOp::URShift)),
            '>' if self.input.skips(['>', '>']) => Ok(Token::Op(Op::URShift)),
            '>' if self.input.skip('>') => Ok(Token::Op(Op::RShift)),
            '<' if self.input.skip('=') => Ok(Token::Op(Op::LessThanOrEqual)),
            '>' if self.input.skip('=') => Ok(Token::Op(Op::GreaterThanOrEqual)),
            '<' => Ok(Token::Op(Op::LessThan)),
            '>' => Ok(Token::Op(Op::GreaterThan)),
            '=' if self.input.skips(['=', '=']) => Ok(Token::Op(Op::StrictEqual)),
            '=' if self.input.skip('>') => Ok(Token::Arrow),
            '=' if self.input.skip('=') => Ok(Token::Op(Op::Equal)),
            '=' => Ok(Token::AssignOp(AssignOp::Normal)),
            '!' if self.input.skips(['=', '=']) => Ok(Token::Op(Op::StrictNotEqual)),
            '!' if self.input.skip('=') => Ok(Token::Op(Op::NotEqual)),
            '!' => Ok(Token::Op(Op::Exclamation)),
            '?' if self.input.skips(['?', '=']) => Ok(Token::AssignOp(AssignOp::NullishCoalescing)),
            '?' if self.input.skip('?') => Ok(Token::Op(Op::NullishCoalescing)),
            '?' => Ok(Token::Op(Op::Question)),
            _ => Err(LexerError::Todo),
        }
    }

    fn read_whitespaces(&mut self) -> Result<Token, LexerError> {
        let s = self.input.take_while(|&c| is_whitespace(c));
        Ok(Token::Whitespace(s))
    }

    fn read_line_terminators(&mut self) -> Result<Token, LexerError> {
        let s = self
            .input
            .take_while(|c| c == &'\n' || c == &'\r' || c == &'\u{2028}' || c == &'\u{2029}');
        Ok(Token::LineTerminator(s))
    }

    fn read_comments(&mut self) -> Result<Token, LexerError> {
        let (c1, c2) = self.input.cur2().unwrap();
        match (c1, c2) {
            ('/', '/') => {
                let s = self
                    .input
                    .take_while(|&c| c != '\n' && c != '\r' && c != '\u{2028}' && c != '\u{2029}');
                assert!(is_line_terminator(self.input.advance().unwrap()));
                Ok(Token::Comment(Comment::SingleLine(s[2..].into())))
            }
            ('/', '*') => {
                let mut last_char = '\0';
                let s = self.input.take_while(|&c| {
                    let is_end = last_char == '*' && c == '/';
                    last_char = c;
                    !is_end
                });
                assert!(self.input.advance().unwrap() == '/');
                Ok(Token::Comment(Comment::MultiLine(s[2..s.len() - 1].into())))
            }
            _ => unreachable!(),
        }
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

    pub fn cur2(&self) -> Option<(char, char)> {
        let mut chars = self.chars.clone().peekable();
        let c1 = chars.next()?;
        let c2 = chars.next()?;
        Some((c1, c2))
    }

    pub fn skips<const N: usize>(&mut self, cs: [char; N]) -> bool {
        let mut chars = self.chars.clone().peekable();
        for expect in cs {
            if chars.peek() == Some(&expect) {
                chars.next();
                continue;
            }
            return false;
        }
        for _ in 0..cs.len() {
            self.advance();
        }
        true
    }

    pub fn skip(&mut self, c: char) -> bool {
        if self.cur() == Some(c) {
            self.advance();
            true
        } else {
            false
        }
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

    pub fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos_in_chars += c.len_utf8();
        Some(c)
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

    #[test]
    fn lex_line_terms() {
        let source = Source::new(
            SourceName::FileName("test.js".into()),
            "if\na b\r\nwhile return",
        );
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn lex_single_line_comment() {
        let source = Source::new(
            SourceName::FileName("test.js".into()),
            "hello//comment\nworld",
        );
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn lex_multi_line_comment() {
        let source = Source::new(
            SourceName::FileName("test.js".into()),
            "hello /* comment コメント */ \n    world",
        );
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn lex_punct() {
        let source = Source::new(SourceName::FileName("test.js".into()), "a + b-c");
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn lex_punct_2() {
        let source = Source::new(SourceName::FileName("test.js".into()), "a++ + --c");
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn lex_punct_3() {
        let source = Source::new(SourceName::FileName("test.js".into()), "{ ( ) [ ] . ... ; , < > <= >= == != === !==
+ - * % ** ++ -- << >> >>> & | ^ ! ~ && || ?? ? : = += -= *= /= %= **= <<= >>= >>>= &= |= ^= &&= ||= ??= =>");
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn lex_while_break() {
        let source = Source::new(
            SourceName::FileName("test.js".into()),
            "while (true) { break; }",
        );
        let mut lexer = Lexer::new(Input::from(&source));
        let mut tokens = vec![];
        while let Ok(Some(token)) = lexer.read_token() {
            tokens.push(token);
        }
        insta::assert_debug_snapshot!(tokens);
    }
}
