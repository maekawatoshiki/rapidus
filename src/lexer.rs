use token::{convert_reserved_keyword, Kind, Symbol, Token};

use std::collections::VecDeque;

#[derive(Clone, Debug)]
pub struct Lexer {
    pub code: String,
    pub pos: usize,
    pub line: usize,
    pub buf: VecDeque<Token>,
    pub pos_line_list: Vec<(usize, usize)>, // pos, line
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
    pub fn next(&mut self) -> Result<Token, ()> {
        match self.read_token() {
            Ok(ref tok) if tok.kind == Kind::LineTerminator => self.next(),
            otherwise => otherwise,
        }
    }

    pub fn peek(&mut self) -> Result<Token, ()> {
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

    pub fn unget(&mut self, tok: &Token) {
        self.buf.push_back(tok.clone());
    }

    pub fn read_token(&mut self) -> Result<Token, ()> {
        if !self.buf.is_empty() {
            return Ok(self.buf.pop_front().unwrap());
        }

        if self.starts_with("//") {
            self.skip_line_comment()?;
        } else if self.starts_with("/*") {
            self.skip_normal_comment()?;
        }

        match self.next_char()? {
            'a'...'z' | 'A'...'Z' | '_' => self.read_identifier(),
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
    fn skip_line_comment(&mut self) -> Result<(), ()> {
        self.just_skip_while(|c| c != '\n')
    }

    fn skip_normal_comment(&mut self) -> Result<(), ()> {
        let mut last_char_is_asterisk = false;
        self.just_skip_while(|c| {
            let end_of_comment = last_char_is_asterisk && c == '/';
            if !end_of_comment {
                last_char_is_asterisk = c == '*';
            }
            !end_of_comment
        })?;
        assert_eq!(self.skip_char()?, '/');
        Ok(())
    }
}

impl Lexer {
    fn read_identifier(&mut self) -> Result<Token, ()> {
        let pos = self.pos;
        let ident = self.skip_while(|c| c.is_alphanumeric() || c == '_')?;
        self.pos_line_list.push((pos, self.line));
        if let Some(keyword) = convert_reserved_keyword(ident.as_str()) {
            Ok(Token::new_keyword(keyword, pos))
        } else {
            Ok(Token::new_identifier(ident, pos))
        }
    }
}

impl Lexer {
    pub fn read_number(&mut self) -> Result<Token, ()> {
        let pos = self.pos;
        let mut is_float = false;
        let mut last = self.next_char()?;
        let num = self.skip_while(|c| {
            is_float = is_float || c == '.';
            let is_f = "eEpP".contains(last) && "+-".contains(c);
            let is_end_of_num = !c.is_alphanumeric() && c != '.' && !is_f;
            if is_end_of_num {
                is_float = is_float || is_f;
            } else {
                last = c;
            }
            !is_end_of_num
        })?;

        let num: f64 = if is_float {
            num.parse().unwrap()
        } else if num.len() > 2 && num.chars().nth(1).unwrap() == 'x' {
            self.read_hex_num(&num[2..]) as f64
        } else if num.len() > 2 && num.chars().nth(1).unwrap() == 'b' {
            self.read_bin_num(&num[2..]) as f64
        } else if num.chars().nth(0).unwrap() == '0' {
            self.read_oct_num(&num[1..]) as f64
        } else {
            self.read_dec_num(num.as_str()) as f64
        };

        self.pos_line_list.push((pos, self.line));
        Ok(Token::new_number(num, pos))
    }

    fn read_hex_num(&mut self, num_literal: &str) -> i64 {
        num_literal.chars().fold(0, |n, c| match c {
            '0'...'9' | 'A'...'F' | 'a'...'f' => n * 16 + c.to_digit(16).unwrap() as i64,
            _ => n,
        })
    }

    fn read_dec_num(&mut self, num_literal: &str) -> i64 {
        num_literal.chars().fold(0, |n, c| match c {
            '0'...'9' => n * 10 + c.to_digit(10).unwrap() as i64,
            _ => n,
        })
    }

    fn read_oct_num(&mut self, num_literal: &str) -> i64 {
        num_literal.chars().fold(0, |n, c| match c {
            '0'...'7' => n * 8 + c.to_digit(8).unwrap() as i64,
            _ => n,
        })
    }
    fn read_bin_num(&mut self, num_literal: &str) -> i64 {
        num_literal.chars().fold(0, |n, c| match c {
            '0' | '1' => n * 2 + c.to_digit(2).unwrap() as i64,
            _ => n,
        })
    }
}

impl Lexer {
    pub fn read_string_literal(&mut self) -> Result<Token, ()> {
        let pos = self.pos;
        let quote = self.skip_char()?;
        // TODO: support escape sequence
        let mut s = "".to_string();
        loop {
            match self.skip_char()? {
                q if q == quote => break,
                '\\' => s.push(self.read_escaped_char()?),
                c => s.push(c),
            }
        }
        self.pos_line_list.push((pos, self.line));
        Ok(Token::new_string(s, pos))
    }

    fn read_escaped_char(&mut self) -> Result<char, ()> {
        let c = self.skip_char()?;
        match c {
            '\'' | '"' | '?' | '\\' => Ok(c),
            'a' => Ok('\x07'),
            'b' => Ok('\x08'),
            'f' => Ok('\x0c'),
            'n' => Ok('\x0a'),
            'r' => Ok('\x0d'),
            't' => Ok('\x09'),
            'v' => Ok('\x0b'),
            'x' => {
                let mut hex = self.skip_while(|c| c.is_alphanumeric())?;
                Ok(self.read_hex_num(hex.as_str()) as u8 as char)
            }
            // TODO: Support unicode codepoint.
            _ => Ok(c),
        }
    }
}

impl Lexer {
    pub fn read_symbol(&mut self) -> Result<Token, ()> {
        let pos = self.pos;
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
                        return Err(());
                    }
                } else {
                    symbol = Symbol::Point
                }
            }
            _ => {}
        };

        self.pos_line_list.push((pos, self.line));
        Ok(Token::new_symbol(symbol, pos))
    }
}

impl Lexer {
    pub fn read_line_terminator(&mut self) -> Result<Token, ()> {
        let pos = self.pos;
        assert_eq!(self.skip_char()?, '\n');
        self.line += 1;
        Ok(Token::new_line_terminator(pos))
    }
}

impl Lexer {
    fn skip_whitespace(&mut self) -> Result<(), ()> {
        self.skip_while(char::is_whitespace).and(Ok(()))
    }

    fn skip_while<F>(&mut self, mut f: F) -> Result<String, ()>
    where
        F: FnMut(char) -> bool,
    {
        let mut s = "".to_string();
        while !self.eof() && f(self.next_char()?) {
            s.push(self.skip_char()?);
        }
        Ok(s)
    }

    fn just_skip_while<F>(&mut self, mut f: F) -> Result<(), ()>
    where
        F: FnMut(char) -> bool,
    {
        while !self.eof() && f(self.next_char()?) {
            self.skip_char()?;
        }
        Ok(())
    }

    fn skip_char(&mut self) -> Result<char, ()> {
        let mut iter = self.code[self.pos..].char_indices();
        let (_, cur_char) = iter.next().ok_or(())?;
        let (next_pos, _) = iter.next().unwrap_or((1, ' '));
        self.pos += next_pos;
        Ok(cur_char)
    }

    fn skip_char_if_any(&mut self, c: char) -> Result<bool, ()> {
        let f = !self.eof() && self.next_char()? == c;
        if f {
            assert_eq!(self.skip_char()?, c);
        }
        Ok(f)
    }

    fn starts_with(&self, s: &str) -> bool {
        self.code[self.pos..].starts_with(s)
    }

    fn next_char(&self) -> Result<char, ()> {
        self.code[self.pos..].chars().next().ok_or(())
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.code.len()
    }
}

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
    let mut lexer = Lexer::new("1 2 0x34 056 7.89 0b10".to_string());
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(1.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(2.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(52.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(46.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(7.89));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(2.0));
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
    let mut lexer = Lexer::new("\"\\' \\\" \\\\ \\a \\b \\f \\n \\r \\t \\v \\x12\"".to_string());
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::String("\' \" \\ \x07 \x08 \x0c \n \r \t \x0b \x12".to_string())
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
        lexer.next().unwrap().kind,
        Kind::Identifier("x".to_string())
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Semicolon));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("y".to_string())
    );
}
