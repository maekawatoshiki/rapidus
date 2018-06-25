use token::{Symbol, Token};

#[derive(Clone, Debug)]
pub struct Lexer {
    pub code: String,
    pub pos: usize,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        Lexer { code: code, pos: 0 }
    }
}

impl Lexer {
    pub fn next(&mut self) -> Result<Token, ()> {
        self.read_token()
    }

    pub fn read_token(&mut self) -> Result<Token, ()> {
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
        Ok(Token::new_identifier(ident))
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
                    symbol = Symbol::Eq
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
                    symbol = Symbol::Ne
                } else {
                    symbol = Symbol::Not
                }
            }
            '<' | '>' | '&' | '|' => {
                let mut single = true;
                if self.skip_char_if_any(c)? {
                    symbol = match c {
                        '<' => Symbol::Shl,
                        '>' => Symbol::Shr,
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

    fn eof(&self) -> bool {
        self.pos >= self.code.len()
    }
}
