use token::{convert_reserved_keyword, Kind, Symbol, Token};

use std::collections::VecDeque;

#[derive(Clone, Debug)]
pub struct Lexer {
    code: String,
    pos: usize,
    buf: VecDeque<Token>,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        Lexer {
            code: code,
            pos: 0,
            buf: VecDeque::new(),
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
    pub fn read_identifier(&mut self) -> Result<Token, ()> {
        let ident = self.skip_while(|c| c.is_alphanumeric() || c == '_')?;
        if let Some(keyword) = convert_reserved_keyword(ident.as_str()) {
            Ok(Token::new_keyword(keyword))
        } else {
            Ok(Token::new_identifier(ident))
        }
    }
}

impl Lexer {
    pub fn read_number(&mut self) -> Result<Token, ()> {
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

        Ok(Token::new_number(num))
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
        let quote = self.skip_char()?;
        // TODO: support escape sequence
        let s = self.skip_while(|c| c != quote)?;
        assert_eq!(self.skip_char()?, quote);
        Ok(Token::new_string(s))
    }
}

impl Lexer {
    pub fn read_symbol(&mut self) -> Result<Token, ()> {
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
            '.' => symbol = Symbol::Point,
            ',' => symbol = Symbol::Comma,
            ';' => symbol = Symbol::Semicolon,
            ':' => symbol = Symbol::Colon,
            '~' => symbol = Symbol::BitwiseNot,
            '?' => symbol = Symbol::Question,
            '#' => symbol = Symbol::Hash,
            _ => {}
        };
        Ok(Token::new_symbol(symbol))
    }
}

impl Lexer {
    pub fn read_line_terminator(&mut self) -> Result<Token, ()> {
        assert_eq!(self.skip_char()?, '\n');
        Ok(Token::new_line_terminator())
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

    fn next_char(&self) -> Result<char, ()> {
        self.code[self.pos..].chars().next().ok_or(())
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.code.len()
    }
}

#[test]
fn number() {
    let mut lexer = Lexer::new("1 2 0x34 056 7.89 0b10".to_string());
    assert_eq!(lexer.next().unwrap(), Token::new_number(1.0));
    assert_eq!(lexer.next().unwrap(), Token::new_number(2.0));
    assert_eq!(lexer.next().unwrap(), Token::new_number(52.0));
    assert_eq!(lexer.next().unwrap(), Token::new_number(46.0));
    assert_eq!(lexer.next().unwrap(), Token::new_number(7.89));
    assert_eq!(lexer.next().unwrap(), Token::new_number(2.0));
}

#[test]
fn identifier() {
    let mut lexer = Lexer::new("console log".to_string());
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_identifier("console".to_string())
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_identifier("log".to_string())
    );
}

#[test]
fn string() {
    let mut lexer = Lexer::new("'aaa' \"bbb\"".to_string());
    assert_eq!(lexer.next().unwrap(), Token::new_string("aaa".to_string()));
    assert_eq!(lexer.next().unwrap(), Token::new_string("bbb".to_string()));
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
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Break,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Case,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Catch,));
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_keyword(Keyword::Continue,)
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_keyword(Keyword::Debugger,)
    );
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Default,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Delete,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Do,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Else,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Finally,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::For,));
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_keyword(Keyword::Function,)
    );
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::If,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::In,));
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_keyword(Keyword::Instanceof,)
    );
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::New,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Return,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Switch,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::This,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Throw,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Try,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Typeof,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Var,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::Void,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::While,));
    assert_eq!(lexer.next().unwrap(), Token::new_keyword(Keyword::With,));
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
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::OpeningParen,)
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::ClosingParen,)
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::OpeningBrace,)
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::ClosingBrace,)
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::OpeningBoxBracket,)
    );
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::ClosingBoxBracket,)
    );
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Comma,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Semicolon,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Colon,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Point,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Arrow,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Inc,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Dec,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Add,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Sub,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Asterisk,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Div,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Mod,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Exp,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Not,));
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::BitwiseNot,)
    );
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Shl,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Shr,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::ZFShr,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Lt,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Le,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Gt,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Ge,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Eq,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Ne,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::SEq,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::SNe,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::And,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Or,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Xor,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::LAnd,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::LOr,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Question,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Assign,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignAdd,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignSub,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignMul,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignDiv,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignMod,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignShl,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignShr,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignAnd,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignOr,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignXor,));
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_symbol(Symbol::AssignLAnd,)
    );
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::AssignLOr,));
    assert_eq!(lexer.next().unwrap(), Token::new_symbol(Symbol::Hash,));
}

#[test]
fn line_terminator() {
    let mut lexer = Lexer::new("hello\nworld".to_string());
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_identifier("hello".to_string())
    );
    assert_eq!(lexer.read_token().unwrap(), Token::new_line_terminator());
    assert_eq!(
        lexer.next().unwrap(),
        Token::new_identifier("world".to_string())
    );
}
