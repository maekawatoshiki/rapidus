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
