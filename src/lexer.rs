use parser::Error;
use token::{convert_reserved_keyword, Kind, Symbol, Token};

use std::collections::VecDeque;

use encoding::all::UTF_16BE;
use encoding::{DecoderTrap, Encoding};

#[derive(Clone, Debug)]
pub struct Lexer {
    pub code: String,
    pub pos: usize,
    pub line: usize,
    pub buf: VecDeque<Token>,
    pub pos_line_list: Vec<(usize, usize)>, // pos, line // TODO: Delete this and consider another way.
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        Lexer {
            code: code,
            pos: 0,
            line: 1,
            buf: VecDeque::new(),
            pos_line_list: vec![],
        }
    }
}

impl Lexer {
    pub fn next(&mut self) -> Result<Token, Error> {
        self.read_token()
    }

    pub fn next_except_lineterminator(&mut self) -> Result<Token, Error> {
        match self.read_token() {
            Ok(ref tok) if tok.kind == Kind::LineTerminator => self.next_except_lineterminator(),
            otherwise => otherwise,
        }
    }

    pub fn peek(&mut self) -> Result<Token, Error> {
        let tok = self.read_token()?;
        self.buf.push_back(tok.clone());
        Ok(tok)
    }

    pub fn skip(&mut self, kind: Kind) -> bool {
        match self.next() {
            Ok(tok) => {
                let success = tok.kind == kind;
                if !success {
                    self.unget(&tok)
                }
                success
            }
            Err(_) => false,
        }
    }

    pub fn skip_except_lineterminator(&mut self, kind: Kind) -> bool {
        match self.next_except_lineterminator() {
            Ok(tok) => {
                let success = tok.kind == kind;
                if !success {
                    self.unget(&tok)
                }
                success
            }
            Err(_) => false,
        }
    }

    pub fn unget(&mut self, tok: &Token) {
        self.buf.push_back(tok.clone());
    }

    pub fn read_token(&mut self) -> Result<Token, Error> {
        if !self.buf.is_empty() {
            return Ok(self.buf.pop_front().unwrap());
        }

        if self.starts_with("//") {
            self.skip_line_comment()?;
        } else if self.starts_with("/*") {
            self.skip_normal_comment()?;
        }

        match self.next_char()? {
            'a'...'z' | 'A'...'Z' | '_' | '$' => self.read_identifier(),
            '0'...'9' => self.read_number(),
            '\'' | '\"' => self.read_string_literal(),
            '\n' => self.read_line_terminator(),
            c if c.is_whitespace() => {
                self.skip_whitespace()?;
                self.read_token()
            }
            _ => self.read_symbol(),
        }
    }
}

impl Lexer {
    fn skip_line_comment(&mut self) -> Result<(), Error> {
        self.just_skip_while(|c| c != '\n')
    }

    fn skip_normal_comment(&mut self) -> Result<(), Error> {
        let mut last_char = ' ';
        let mut line = self.line;
        self.just_skip_while(|c| {
            if c == '\n' {
                line += 1;
            }
            let end_of_comment = last_char == '*' && c == '/';
            last_char = c;
            !end_of_comment
        })?;
        self.line = line;
        assert_eq!(self.skip_char()?, '/');
        Ok(())
    }
}

impl Lexer {
    fn read_identifier(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        self.pos_line_list.push((pos, self.line));
        let ident = self.skip_while(|c| c.is_alphanumeric() || c == '_' || c == '$')?;
        if let Some(keyword) = convert_reserved_keyword(ident.as_str()) {
            Ok(Token::new_keyword(keyword, pos))
        } else {
            Ok(Token::new_identifier(ident, pos))
        }
    }
}

impl Lexer {
    pub fn read_number(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        self.pos_line_list.push((pos, self.line));

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

        match self.skip_char()? {
            '0' if self.eof() => return Ok(Token::new_number(0.0, pos)),
            '0' => {
                let c = self.next_char()?;
                match c {
                    'x' | 'X' => kind = NumLiteralKind::Hex,
                    'b' | 'B' => kind = NumLiteralKind::Bin,
                    'o' | 'O' => kind = NumLiteralKind::Oct,
                    '0'...'7' => {
                        kind = NumLiteralKind::OldOct;
                        num_literal.push(c);
                    }
                    '.' => num_literal.push('.'),
                    '8'...'9' => num_literal.push(c),
                    _ => return Ok(Token::new_number(0.0, pos)),
                }
                self.skip_char()?;
            }
            c => num_literal.push(c),
        }

        macro_rules! read_num {
            ( $($valid_chars:pat),* ) => {
                self.skip_while(|c| match c.to_ascii_lowercase() {
                    $( $valid_chars )|* => true,
                    _ => false }
                )?
            };
        }

        num_literal += match kind {
            NumLiteralKind::Hex => read_num!('0'...'9', 'a'...'f'),
            NumLiteralKind::Oct => read_num!('0'...'7'),
            NumLiteralKind::Bin => read_num!('0'...'1'),
            NumLiteralKind::OldOct => self.skip_while(|c| match c {
                '0'...'7' => true,
                '8'...'9' => {
                    kind = NumLiteralKind::Dec;
                    true
                }
                _ => false,
            })?,
            NumLiteralKind::Dec => {
                let mut last = ' ';
                self.skip_while(|c| {
                    let is_f = "eE".contains(last) && "+-0123456789".contains(c);
                    last = c;
                    c.is_alphanumeric() || c == '.' || is_f
                })?
            }
        }.as_str();

        let num = match kind {
            NumLiteralKind::Dec => num_literal.parse().unwrap(),
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
                '0'...'9' | 'A'...'F' | 'a'...'f' => n * 16 + c.to_digit(16).unwrap() as i64,
                _ => n,
            })
    }

    fn read_oct_num(&mut self, num_literal: &str) -> i64 {
        num_literal
            .chars()
            .fold(0, |n, c| match c.to_ascii_lowercase() {
                '0'...'7' => n * 8 + c.to_digit(8).unwrap() as i64,
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
    pub fn read_string_literal(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        self.pos_line_list.push((pos, self.line));
        let quote = self.skip_char()?;
        // TODO: support escape sequence
        let mut s = "".to_string();
        loop {
            match self.skip_char()? {
                q if q == quote => break,
                '\\' => for c in self.read_escaped_char()? {
                    s.push(c)
                },
                c => s.push(c),
            }
        }
        Ok(Token::new_string(s, pos))
    }

    fn read_escaped_char(&mut self) -> Result<Vec<char>, Error> {
        let c = self.skip_char()?;
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
                let hex = self.skip_while(|c| c.is_alphanumeric())?;
                vec![self.read_hex_num(hex.as_str()) as u8 as char]
            }
            'u' => {
                let mut u8s = vec![];
                loop {
                    let hex = self.skip_while(|c| c.is_alphanumeric())?;
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
                    if self.skip_char()? == '\\' && self.skip_char()? == 'u' {
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
        self.pos_line_list.push((pos, self.line));

        let mut symbol = Symbol::Hash;
        let c = self.skip_char()?;
        match c {
            '+' | '-' => match self.next_char()? {
                '=' => {
                    assert_eq!(self.skip_char()?, '=');
                    if c == '+' {
                        symbol = Symbol::AssignAdd;
                    } else if c == '-' {
                        symbol = Symbol::AssignSub;
                    }
                }
                '>' => {
                    assert_eq!(self.skip_char()?, '>');
                    if c == '-' {
                        symbol = Symbol::Arrow;
                    }
                }
                '+' => {
                    assert_eq!(self.skip_char()?, '+');
                    if c == '+' {
                        symbol = Symbol::Inc;
                    }
                }
                '-' => {
                    assert_eq!(self.skip_char()?, '-');
                    if c == '-' {
                        symbol = Symbol::Dec;
                    }
                }
                _ => if c == '+' {
                    symbol = Symbol::Add;
                } else if c == '-' {
                    symbol = Symbol::Sub;
                },
            },
            '*' => {
                if self.skip_char_if_any('=')? {
                    symbol = Symbol::AssignMul
                } else if self.skip_char_if_any('*')? {
                    symbol = Symbol::Exp
                } else {
                    symbol = Symbol::Asterisk
                }
            }
            '/' => {
                if self.skip_char_if_any('=')? {
                    symbol = Symbol::AssignDiv
                } else {
                    symbol = Symbol::Div
                }
            }
            '%' => {
                if self.skip_char_if_any('=')? {
                    symbol = Symbol::AssignMod
                } else {
                    symbol = Symbol::Mod
                }
            }
            '=' => {
                if self.skip_char_if_any('=')? {
                    symbol = if self.skip_char_if_any('=')? {
                        Symbol::SEq
                    } else {
                        Symbol::Eq
                    }
                } else {
                    symbol = Symbol::Assign
                }
            }
            '^' => {
                if self.skip_char_if_any('=')? {
                    symbol = Symbol::AssignXor
                } else {
                    symbol = Symbol::Xor
                }
            }
            '!' => {
                if self.skip_char_if_any('=')? {
                    symbol = if self.skip_char_if_any('=')? {
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
                if self.skip_char_if_any(c)? {
                    symbol = match c {
                        '<' => Symbol::Shl,
                        '>' => {
                            if self.skip_char_if_any('>')? {
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
                if self.skip_char_if_any('=')? {
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
                if self.skip_char_if_any('.')? {
                    symbol = if self.skip_char_if_any('.')? {
                        Symbol::Rest
                    } else {
                        // TODO: better error handler needed
                        return Err(Error::UnexpectedToken(
                            pos,
                            ErrorMsgKind::Normal,
                            "expected '...'".to_string(),
                        ));
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
    pub fn read_line_terminator(&mut self) -> Result<Token, Error> {
        let pos = self.pos;
        assert_eq!(self.skip_char()?, '\n');
        self.line += 1;
        Ok(Token::new_line_terminator(pos))
    }
}

impl Lexer {
    fn skip_whitespace(&mut self) -> Result<(), Error> {
        self.skip_while(|c| c == ' ' || c == '\t').and(Ok(()))
    }

    fn skip_while<F>(&mut self, mut f: F) -> Result<String, Error>
    where
        F: FnMut(char) -> bool,
    {
        let mut s = "".to_string();
        while !self.eof() && f(self.next_char()?) {
            s.push(self.skip_char()?);
        }
        Ok(s)
    }

    fn just_skip_while<F>(&mut self, mut f: F) -> Result<(), Error>
    where
        F: FnMut(char) -> bool,
    {
        while !self.eof() && f(self.next_char()?) {
            self.skip_char()?;
        }
        Ok(())
    }

    fn skip_char(&mut self) -> Result<char, Error> {
        let mut iter = self.code[self.pos..].char_indices();
        let (_, cur_char) = iter.next().ok_or(Error::NormalEOF)?;
        let (next_pos, _) = iter.next().unwrap_or((cur_char.len_utf8(), ' '));
        self.pos += next_pos;
        Ok(cur_char)
    }

    fn skip_char_if_any(&mut self, c: char) -> Result<bool, Error> {
        let f = !self.eof() && self.next_char()? == c;
        if f {
            assert_eq!(self.skip_char()?, c);
        }
        Ok(f)
    }

    fn starts_with(&self, s: &str) -> bool {
        self.code[self.pos..].starts_with(s)
    }

    fn next_char(&self) -> Result<char, Error> {
        self.code[self.pos..].chars().next().ok_or(Error::NormalEOF)
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.code.len()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ErrorMsgKind {
    Normal,
    LastToken,
}

impl Lexer {
    pub fn get_code_around_err_point(&self, mut pos: usize, kind: ErrorMsgKind) -> (String, usize) {
        match kind {
            ErrorMsgKind::LastToken => {
                let mut last = 0;
                self.pos_line_list.iter().find(|(p, _)| {
                    let is = *p == pos;
                    if !is {
                        last = *p
                    };
                    is
                });
                pos = last;
            }
            _ => {}
        };
        let code = self.code.as_bytes();
        let start_pos = {
            let mut p = pos as i32;
            while p > 0 && code[p as usize] != b'\n' {
                p -= 1;
            }
            if code[p as usize] == b'\n' {
                p += 1
            }
            p as usize
        };
        let end_pos = {
            let mut p = start_pos as i32;
            while p < code.len() as i32 && code[p as usize] as char != '\n' {
                p += 1;
            }
            p as usize
        };
        let surrounding_code = String::from_utf8(code[start_pos..end_pos].to_vec())
            .unwrap()
            .to_string();
        let mut err_point = String::new();
        for _ in 0..(pos as i32 - start_pos as i32).abs() {
            err_point.push(' ');
        }
        err_point.push('^');
        (surrounding_code + "\n" + err_point.as_str(), pos)
    }
}

#[test]
fn number() {
    let mut lexer = Lexer::new("1 2 0x34 056 7.89 0b10 5e3 5e+3 5e-3 0999 0O123".to_string());
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
    assert_eq!(lexer.next().unwrap().kind, Kind::String("aaa".to_string()));
    assert_eq!(lexer.next().unwrap().kind, Kind::String("bbb".to_string()));
}

#[test]
fn keyword() {
    use token::Keyword;

    let mut lexer = Lexer::new(
        "break case catch continue debugger default \
         delete do else finally for function if in instanceof \
         new return switch this throw try typeof \
         var void while with"
            .to_string(),
    );
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
}

#[test]
fn line_terminator() {
    let mut lexer = Lexer::new("hello\nworld".to_string());
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
    assert_eq!(
        lexer.next_except_lineterminator().unwrap().kind,
        Kind::Identifier("x".to_string())
    );
    assert_eq!(
        lexer.next_except_lineterminator().unwrap().kind,
        Kind::Symbol(Symbol::Semicolon)
    );
    assert_eq!(
        lexer.next_except_lineterminator().unwrap().kind,
        Kind::Identifier("y".to_string())
    );
}
