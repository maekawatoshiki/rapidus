use crate::parser::Error;
use crate::token::{convert_reserved_keyword, Kind, Symbol, Token};

use std::collections::VecDeque;

use encoding::all::UTF_16BE;
use encoding::{DecoderTrap, Encoding};

#[derive(Clone, Debug)]
pub struct Lexer {
    pub code: String,

    /// Current positon in code.
    /// After tokenizing, always indicate EOF.
    pub pos: usize,

    /// Current line number
    pub line: usize,

    /// Hold all tokens
    pub buf: VecDeque<Token>,

    /// Associate position in ``code`` and line number
    // TODO: Delete this and consider another way.
    pub pos_line_list: Vec<(usize, usize)>,

    /// Current position in ``buf``.
    pub token_pos: usize,

    /// Previous position of ``token_pos``
    pub prev_token_pos: usize,

    /// Saved states
    pub states: Vec<usize>,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        Lexer {
            code: code,
            pos: 0,
            line: 1,
            buf: VecDeque::new(),
            pos_line_list: vec![(0, 1)],
            token_pos: 0,
            prev_token_pos: 0,
            states: vec![],
        }
    }

    /// Tokenize all the script
    pub fn tokenize_all(&mut self) -> Result<(), Error> {
        loop {
            match self.tokenize() {
                Ok(tok) => self.buf.push_back(tok),
                Err(Error::NormalEOF) => break,
                Err(err) => {
                    // When error occurs in tokenizer, pos_line_list is not completed.
                    self.skip_char_while(|c| c != '\n')?;
                    self.take_char().unwrap_or(' ');
                    self.pos_line_list.push((self.pos + 1, self.line + 1));
                    return Err(err);
                }
            };
        }

        self.pos_line_list.push((self.pos + 1, self.line + 1));

        let mut prev_pos = 0;
        for mut tok in &mut self.buf {
            tok.prev_pos = prev_pos;
            prev_pos = tok.pos;
        }

        Ok(())
    }

    pub fn print_buf(&self) {
        for tok in &self.buf {
            println!("{:?}", tok);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.token_pos >= self.buf.len()
    }
}

impl Lexer {
    /// Get next token.
    /// No skipping line terminator.
    pub fn next(&mut self) -> Result<Token, Error> {
        self.prev_token_pos = self.token_pos;
        self.read_token()
    }

    /// Get the next token.
    /// Skipping line terminators.
    pub fn next_skip_lineterminator(&mut self) -> Result<Token, Error> {
        self.prev_token_pos = self.token_pos;
        loop {
            let tok = self.read_token()?;
            if tok.kind != Kind::LineTerminator {
                return Ok(tok);
            }
        }
    }

    /// Skip line terminators.
    /// Return Err(Error::NormalEOF) when reached EOF.
    pub fn skip_lineterminator(&mut self) -> Result<(), Error> {
        let len = self.buf.len();
        for i in self.token_pos..len {
            let tok = self.buf[i].clone();
            if tok.kind != Kind::LineTerminator {
                self.token_pos = i;
                return Ok(());
            }
        }
        Err(Error::NormalEOF)
    }

    /// Peek the next token.
    /// Skipping line terminators.
    pub fn peek_skip_lineterminator(&mut self) -> Result<Token, Error> {
        let len = self.buf.len();
        for i in self.token_pos..len {
            let tok = self.buf[i].clone();
            if tok.kind != Kind::LineTerminator {
                return Ok(tok);
            }
        }
        Err(Error::NormalEOF)
    }

    /// Peek the token specified by index.
    /// Return the next token when index = 0.
    pub fn peek(&mut self, index: usize) -> Result<Token, Error> {
        let index_in_buf = self.token_pos + index;
        if index_in_buf < self.buf.len() {
            Ok(self.buf[index_in_buf].clone())
        } else {
            Err(Error::NormalEOF)
        }
    }

    /// Peek the previous token
    pub fn peek_prev(&mut self) -> Token {
        let index_in_buf = self.token_pos - 1;
        self.buf[index_in_buf].clone()
    }

    /// Get char position in the script of the next token
    pub fn get_current_pos(&mut self) -> usize {
        if self.token_pos < self.buf.len() {
            self.buf[self.token_pos].pos
        } else {
            self.pos
        }
    }

    /// Get char position in the script of previous token.
    pub fn get_prev_pos(&mut self) -> usize {
        if self.token_pos < self.buf.len() {
            self.buf[self.token_pos].prev_pos
        } else {
            self.pos - 1
        }
    }

    /// Peek the next token and if it is ``kind``, get the next token, return true.
    /// Otherwise, return false.
    pub fn next_if(&mut self, kind: Kind) -> bool {
        match self.peek(0) {
            Ok(tok) => {
                if tok.kind == kind {
                    let _ = self.read_token().unwrap();
                    true
                } else {
                    false
                }
            }
            Err(_) => false,
        }
    }

    /// Peek the next token, and when token is kind:Kind, get the token and return true.
    /// Otherwise, return false.
    /// Skipping line terminators.
    pub fn next_if_skip_lineterminator(&mut self, kind: Kind) -> Result<bool, Error> {
        match self.peek_skip_lineterminator() {
            Ok(tok) => {
                if tok.kind == kind {
                    self.next_skip_lineterminator()?;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Err(e) => Err(e),
        }
    }

    /// Revert the previous ``next()`` or ``next_skip_lineterminator()``.
    /// Does not support ``next_if()`` and ``next_if_skip_lineterminator()``.
    pub fn unget(&mut self) {
        self.token_pos = self.prev_token_pos;
    }

    /// Read token
    fn read_token(&mut self) -> Result<Token, Error> {
        if self.token_pos < self.buf.len() {
            let pos = self.token_pos;
            self.token_pos += 1;
            Ok(self.buf[pos].clone())
        } else {
            Err(Error::NormalEOF)
        }
    }
}

///
/// Tokenizer
///
impl Lexer {
    /// Tokenize and return the token
    fn tokenize(&mut self) -> Result<Token, Error> {
        if self.starts_with("//") {
            self.skip_line_comment()?;
        } else if self.starts_with("/*") {
            self.skip_normal_comment()?;
        }

        match self.peek_char()? {
            'a'..='z' | 'A'..='Z' | '_' | '$' => self.read_identifier(),
            '0'..='9' => self.read_number(),
            '\'' | '\"' => self.read_string_literal(),
            '\n' => self.read_line_terminator(),
            c if c.is_whitespace() => {
                self.skip_whitespace()?;
                self.tokenize()
            }
            _ => self.read_symbol(),
        }
    }
}

impl Lexer {
    fn skip_line_comment(&mut self) -> Result<(), Error> {
        self.skip_char_while(|c| c != '\n')
    }

    fn skip_normal_comment(&mut self) -> Result<(), Error> {
        let mut last_char = ' ';
        let mut line = self.line;
        self.skip_char_while(|c| {
            if c == '\n' {
                line += 1;
            }
            let end_of_comment = last_char == '*' && c == '/';
            last_char = c;
            !end_of_comment
        })?;
        if self.line != line {
            self.pos_line_list.push((self.pos, line));
        }
        self.line = line;
        assert_eq!(self.take_char()?, '/');
        Ok(())
    }
}

impl Lexer {
    fn read_identifier(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        let ident = self.take_char_while(|c| c.is_alphanumeric() || c == '_' || c == '$')?;
        if let Some(keyword) = convert_reserved_keyword(ident.as_str()) {
            Ok(Token::new_keyword(keyword, pos))
        } else {
            Ok(Token::new_identifier(ident, pos))
        }
    }
}

impl Lexer {
    fn read_number(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        #[derive(Debug, Clone, PartialEq)]
        enum NumLiteralKind {
            Hex,
            Dec,
            Oct,
            OldOct,
            Bin,
        }

        let mut kind = NumLiteralKind::Dec;
        let mut num_literal = "".to_string();

        match self.take_char()? {
            '0' if self.eof() => return Ok(Token::new_number(0.0, pos)),
            '0' => {
                let c = self.peek_char()?;
                match c {
                    'x' | 'X' => kind = NumLiteralKind::Hex,
                    'b' | 'B' => kind = NumLiteralKind::Bin,
                    'o' | 'O' => kind = NumLiteralKind::Oct,
                    '0'..='7' => {
                        kind = NumLiteralKind::OldOct;
                        num_literal.push(c);
                    }
                    '.' => num_literal.push('.'),
                    '8'..='9' => num_literal.push(c),
                    _ => return Ok(Token::new_number(0.0, pos)),
                }
                self.take_char()?;
            }
            c => num_literal.push(c),
        }

        macro_rules! read_num {
            ( $($valid_chars:pat),* ) => {
                self.take_char_while(|c| match c.to_ascii_lowercase() {
                    $( $valid_chars )|* => true,
                    _ => false }
                )?
            };
        }

        num_literal += match kind {
            NumLiteralKind::Hex => read_num!('0'..='9', 'a'..='f'),
            NumLiteralKind::Oct => read_num!('0'..='7'),
            NumLiteralKind::Bin => read_num!('0'..='1'),
            NumLiteralKind::OldOct => self.take_char_while(|c| match c {
                '0'..='7' => true,
                '8'..='9' => {
                    kind = NumLiteralKind::Dec;
                    true
                }
                _ => false,
            })?,
            NumLiteralKind::Dec => {
                let mut last = ' ';
                self.take_char_while(|c| {
                    let is_f = "eE".contains(last) && "+-0123456789".contains(c);
                    last = c;
                    c.is_alphanumeric() || c == '.' || is_f
                })?
            }
        }
        .as_str();

        let num = match kind {
            NumLiteralKind::Dec => match num_literal.parse() {
                Ok(ok) => ok,
                Err(_) => {
                    return Err(Error::General(pos, "invalid token".to_string()));
                }
            },
            NumLiteralKind::Hex => self.read_hex_num(num_literal.as_str()) as f64,
            NumLiteralKind::Oct | NumLiteralKind::OldOct => {
                self.read_oct_num(num_literal.as_str()) as f64
            }
            NumLiteralKind::Bin => self.read_bin_num(num_literal.as_str()) as f64,
        };

        Ok(Token::new_number(num, pos))
    }

    fn read_hex_num(&mut self, num_literal: &str) -> i64 {
        num_literal
            .chars()
            .fold(0, |n, c| match c.to_ascii_lowercase() {
                '0'..='9' | 'A'..='F' | 'a'..='f' => n * 16 + c.to_digit(16).unwrap() as i64,
                _ => n,
            })
    }

    fn read_oct_num(&mut self, num_literal: &str) -> i64 {
        num_literal
            .chars()
            .fold(0, |n, c| match c.to_ascii_lowercase() {
                '0'..='7' => n * 8 + c.to_digit(8).unwrap() as i64,
                _ => n,
            })
    }

    fn read_bin_num(&mut self, num_literal: &str) -> i64 {
        num_literal
            .chars()
            .fold(0, |n, c| match c.to_ascii_lowercase() {
                '0' | '1' => n * 2 + c.to_digit(2).unwrap() as i64,
                _ => n,
            })
    }
}

impl Lexer {
    fn read_string_literal(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        let quote = self.take_char()?;
        let mut s = "".to_string();
        loop {
            match self.take_char()? {
                q if q == quote => break,
                '\\' => {
                    for c in self.read_escaped_char()? {
                        s.push(c)
                    }
                }
                c => s.push(c),
            }
        }
        Ok(Token::new_string(s, pos))
    }

    // TODO: Support more escape sequences
    fn read_escaped_char(&mut self) -> Result<Vec<char>, Error> {
        let c = self.take_char()?;
        Ok(match c {
            '\'' | '"' | '?' | '\\' => vec![c],
            'a' => vec!['\x07'],
            'b' => vec!['\x08'],
            'f' => vec!['\x0c'],
            'n' => vec!['\x0a'],
            'r' => vec!['\x0d'],
            't' => vec!['\x09'],
            'v' => vec!['\x0b'],
            'x' => {
                let hex = self.take_char_while(|c| c.is_alphanumeric())?;
                vec![self.read_hex_num(hex.as_str()) as u8 as char]
            }
            'u' => {
                let mut u8s = vec![];
                loop {
                    let hex = self.take_char_while(|c| c.is_alphanumeric())?;
                    let mut i = 0;
                    while i < hex.len() {
                        u8s.push(
                            self.read_hex_num(&hex[i..i + if i + 2 > hex.len() { 1 } else { 2 }])
                                as u8,
                        );
                        i += 2;
                    }
                    if u8s.len() % 2 != 0 {
                        // TODO: Support \u{X..X}
                        unimplemented!("unsupported escape sequence");
                    }
                    let save_pos = self.pos;
                    // TODO: Error handling
                    if self.take_char()? == '\\' && self.take_char()? == 'u' {
                        continue;
                    } else {
                        self.pos = save_pos;
                        break;
                    }
                }
                UTF_16BE
                    .decode(u8s.as_slice(), DecoderTrap::Strict)
                    .unwrap()
                    .chars()
                    .collect::<Vec<char>>()
            }
            _ => vec![c],
        })
    }
}

impl Lexer {
    pub fn read_symbol(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        let mut symbol = Symbol::Hash;
        let c = self.take_char()?;
        match c {
            '+' | '-' => match self.peek_char()? {
                '=' => {
                    assert_eq!(self.take_char()?, '=');
                    if c == '+' {
                        symbol = Symbol::AssignAdd;
                    } else if c == '-' {
                        symbol = Symbol::AssignSub;
                    }
                }
                '>' => {
                    assert_eq!(self.take_char()?, '>');
                    if c == '-' {
                        symbol = Symbol::Arrow;
                    }
                }
                '+' => {
                    assert_eq!(self.take_char()?, '+');
                    if c == '+' {
                        symbol = Symbol::Inc;
                    }
                }
                '-' => {
                    assert_eq!(self.take_char()?, '-');
                    if c == '-' {
                        symbol = Symbol::Dec;
                    }
                }
                _ => {
                    if c == '+' {
                        symbol = Symbol::Add;
                    } else if c == '-' {
                        symbol = Symbol::Sub;
                    }
                }
            },
            '*' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignMul
                } else if self.take_char_if('*')? {
                    symbol = Symbol::Exp
                } else {
                    symbol = Symbol::Asterisk
                }
            }
            '/' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignDiv
                } else {
                    symbol = Symbol::Div
                }
            }
            '%' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignMod
                } else {
                    symbol = Symbol::Mod
                }
            }
            '=' => {
                if self.take_char_if('>')? {
                    symbol = Symbol::FatArrow
                } else if self.take_char_if('=')? {
                    symbol = if self.take_char_if('=')? {
                        Symbol::SEq
                    } else {
                        Symbol::Eq
                    }
                } else {
                    symbol = Symbol::Assign
                }
            }
            '^' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignXor
                } else {
                    symbol = Symbol::Xor
                }
            }
            '!' => {
                if self.take_char_if('=')? {
                    symbol = if self.take_char_if('=')? {
                        Symbol::SNe
                    } else {
                        Symbol::Ne
                    }
                } else {
                    symbol = Symbol::Not
                }
            }
            '<' | '>' | '&' | '|' => {
                let mut single = true;
                if self.take_char_if(c)? {
                    symbol = match c {
                        '<' => Symbol::Shl,
                        '>' => {
                            if self.take_char_if('>')? {
                                Symbol::ZFShr
                            } else {
                                Symbol::Shr
                            }
                        }
                        '&' => Symbol::LAnd,
                        '|' => Symbol::LOr,
                        _ => unreachable!(),
                    };
                    single = false;
                }
                if self.take_char_if('=')? {
                    symbol = match (c, symbol) {
                        ('<', Symbol::Shl) => Symbol::AssignShl,
                        ('<', _) => Symbol::Le,
                        ('>', Symbol::Shr) => Symbol::AssignShr,
                        ('>', _) => Symbol::Ge,
                        ('&', Symbol::LAnd) => Symbol::AssignLAnd,
                        ('&', _) => Symbol::AssignAnd,
                        ('|', Symbol::LOr) => Symbol::AssignLOr,
                        ('|', _) => Symbol::AssignOr,
                        _ => unreachable!(),
                    };
                    single = false;
                }
                if single {
                    symbol = match c {
                        '<' => Symbol::Lt,
                        '>' => Symbol::Gt,
                        '&' => Symbol::And,
                        '|' => Symbol::Or,
                        _ => unreachable!(),
                    };
                }
            }
            '(' => symbol = Symbol::OpeningParen,
            ')' => symbol = Symbol::ClosingParen,
            '[' => symbol = Symbol::OpeningBoxBracket,
            ']' => symbol = Symbol::ClosingBoxBracket,
            '{' => symbol = Symbol::OpeningBrace,
            '}' => symbol = Symbol::ClosingBrace,
            ',' => symbol = Symbol::Comma,
            ';' => symbol = Symbol::Semicolon,
            ':' => symbol = Symbol::Colon,
            '~' => symbol = Symbol::BitwiseNot,
            '?' => symbol = Symbol::Question,
            '#' => symbol = Symbol::Hash,
            '.' => {
                if self.take_char_if('.')? {
                    symbol = if self.take_char_if('.')? {
                        Symbol::Rest
                    } else {
                        return Err(Error::General(pos, "Invalid token".to_string()));
                    }
                } else {
                    symbol = Symbol::Point
                }
            }
            _ => {}
        };

        Ok(Token::new_symbol(symbol, pos))
    }
}

impl Lexer {
    /// Read line terminator. (if next char is not line terminator, panic.)
    fn read_line_terminator(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        assert_eq!(self.take_char()?, '\n');
        self.line += 1;
        self.pos_line_list.push((self.pos, self.line));
        Ok(Token::new_line_terminator(pos))
    }
}

impl Lexer {
    /// Skip whitespace and tabs
    fn skip_whitespace(&mut self) -> Result<(), Error> {
        self.take_char_while(|c| c == ' ' || c == '\t').and(Ok(()))
    }

    /// While ``f(char)`` is true, read chars, and move cursor next.
    /// Return all chars as String.
    fn take_char_while<F>(&mut self, mut f: F) -> Result<String, Error>
    where
        F: FnMut(char) -> bool,
    {
        let mut s = "".to_string();
        while !self.eof() && f(self.peek_char()?) {
            s.push(self.take_char()?);
        }
        Ok(s)
    }

    /// While ``f(char)`` is true and not reached EOF, move cursor next
    fn skip_char_while<F>(&mut self, mut f: F) -> Result<(), Error>
    where
        F: FnMut(char) -> bool,
    {
        while !self.eof() && f(self.peek_char()?) {
            self.take_char()?;
        }
        Ok(())
    }

    /// Read next char, and move cursor next
    fn take_char(&mut self) -> Result<char, Error> {
        let mut iter = self.code[self.pos..].char_indices();
        let (_, cur_char) = iter.next().ok_or(Error::NormalEOF)?;
        let (next_pos, _) = iter.next().unwrap_or((cur_char.len_utf8(), ' '));
        self.pos += next_pos;
        Ok(cur_char)
    }

    /// If the next char is ``c``, move cursor next, return true.
    /// If not (include EOF), return false.
    fn take_char_if(&mut self, c: char) -> Result<bool, Error> {
        let f = !self.eof() && self.peek_char()? == c;
        if f {
            assert_eq!(self.take_char()?, c);
        }
        Ok(f)
    }

    /// If chars start with ``s``, return true
    fn starts_with(&self, s: &str) -> bool {
        self.code[self.pos..].starts_with(s)
    }

    /// peek next char. if eof, raise Err(Error::NormalEOF)
    fn peek_char(&self) -> Result<char, Error> {
        self.code[self.pos..].chars().next().ok_or(Error::NormalEOF)
    }

    fn eof(&self) -> bool {
        self.pos >= self.code.len()
    }
}

impl Lexer {
    pub fn get_code_around_err_point(&self, pos: usize) -> (String, usize, usize) {
        let code = self.code.as_bytes();
        let iter = self.pos_line_list.iter();
        let (start_pos, line) = iter.take_while(|x| x.0 <= pos).last().unwrap();

        let mut iter = self.pos_line_list.iter();
        let end_pos = match iter
            .find(|x| x.0 > pos)
            .unwrap_or(self.pos_line_list.last().unwrap())
            .0
        {
            x if x == 0 => 0,
            x => x - 1,
        };
        let surrounding_code = String::from_utf8(code[*start_pos..end_pos].to_vec())
            .unwrap()
            .to_string();
        let err_point = format!("{}{}", " ".repeat(pos - start_pos), '^',);
        (surrounding_code + "\n" + err_point.as_str(), pos, *line)
    }
}

#[test]
fn number() {
    let mut lexer = Lexer::new("1 2 0x34 056 7.89 0b10 5e3 5e+3 5e-3 0999 0O123".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(1.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(2.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(52.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(46.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(7.89));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(2.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(5e3));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(5e+3));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(5e-3));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(999.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(0o123 as f64));
}

#[test]
fn identifier() {
    let mut lexer = Lexer::new("console log".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("console".to_string())
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("log".to_string())
    );
}

#[test]
fn string() {
    let mut lexer = Lexer::new("'aaa' \"bbb\"".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::String("aaa".to_string()));
    assert_eq!(lexer.next().unwrap().kind, Kind::String("bbb".to_string()));
}

#[test]
fn keyword() {
    use crate::token::Keyword;

    let mut lexer = Lexer::new(
        "break case catch continue debugger default \
         delete do else finally for function if in instanceof \
         new return switch this throw try typeof \
         var void while with"
            .to_string(),
    );
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Break,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Case,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Catch,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Continue,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Debugger,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Default,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Delete,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Do,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Else,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Finally,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::For,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Function,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::If,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::In,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Instanceof,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::New,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Return,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Switch,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::This,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Throw,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Try,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Typeof,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Var,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Void,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::While,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::With,));
}

#[test]
fn symbol() {
    let mut lexer = Lexer::new(
        "() {} [] , ; : . -> ++ -- + - * / % **\
         ! ~ << >> >>> < <= > >= == != === !== & | ^ && || \
         ? = += -= *= /= %= <<= >>= &= |= ^= \
         &&= ||= #"
            .to_string(),
    );
    lexer.tokenize_all().unwrap();

    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OpeningParen,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::ClosingParen,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OpeningBrace,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::ClosingBrace,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OpeningBoxBracket,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::ClosingBoxBracket,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Comma,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Semicolon,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Colon,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Point,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Arrow,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Inc,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Dec,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Add,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Sub,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Asterisk,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Div,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Mod,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Exp,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Not,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::BitwiseNot,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Shl,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Shr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::ZFShr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Lt,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Le,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Gt,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Ge,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Eq,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Ne,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::SEq,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::SNe,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::And,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Or,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Xor,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::LAnd,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::LOr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Question,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Assign,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignAdd,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignSub,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignMul,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignDiv,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignMod,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignShl,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignShr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignAnd,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignOr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignXor,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::AssignLAnd,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignLOr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Hash,));
    use crate::token::get_string_for_symbol;
    assert_eq!(
        format!(
            "{}{}{}{}{}{}{}{}{}",
            get_string_for_symbol(Symbol::OpeningBrace),
            get_string_for_symbol(Symbol::ClosingBrace),
            get_string_for_symbol(Symbol::OpeningParen),
            get_string_for_symbol(Symbol::ClosingParen),
            get_string_for_symbol(Symbol::OpeningBoxBracket),
            get_string_for_symbol(Symbol::ClosingBoxBracket),
            get_string_for_symbol(Symbol::Comma),
            get_string_for_symbol(Symbol::Colon),
            get_string_for_symbol(Symbol::Semicolon)
        ),
        "{}()[],:;".to_string()
    );
}

#[test]
fn line_terminator() {
    let mut lexer = Lexer::new("hello\nworld".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("hello".to_string())
    );
    assert_eq!(lexer.read_token().unwrap().kind, Kind::LineTerminator);
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("world".to_string())
    );
}

#[test]
fn escape_seq() {
    let mut lexer = Lexer::new(
        "\"\\' \\\" \\\\ \\a \\b \\f \\n \\r \\t \\v \\x12 \\uD867\\uDE3D\"".to_string(),
    );
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::String("\' \" \\ \x07 \x08 \x0c \n \r \t \x0b \x12 𩸽".to_string())
    );
}

#[test]
fn comment() {
    let mut lexer = Lexer::new(
        "x; // line comment
                               /* multi-line
                                * comment 
                                */
                               y"
        .to_string(),
    );
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next_skip_lineterminator().unwrap().kind,
        Kind::Identifier("x".to_string())
    );
    assert_eq!(
        lexer.next_skip_lineterminator().unwrap().kind,
        Kind::Symbol(Symbol::Semicolon)
    );
    assert_eq!(
        lexer.next_skip_lineterminator().unwrap().kind,
        Kind::Identifier("y".to_string())
    );
}
