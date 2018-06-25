pub struct Lexer {
    pub code: String,
    pub pos: usize,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        Lexer { code: code, pos: 0 }
    }
}
