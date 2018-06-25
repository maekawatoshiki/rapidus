use token::Token;

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
        Ok(Token::new_number(0.0))
    }
}

impl Lexer {
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
