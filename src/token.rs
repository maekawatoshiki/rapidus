pub struct Token {
    pub kind: Kind,
}

pub enum Kind {
    Keyword(Keyword),
    Identifier(String),
    Number(f64),
    String(String),
    Symbol(Symbol),
    Newline,
}

pub enum Keyword {
}

pub enum Symbol {
}

impl Token {
    pub fn new_number(f: f64) -> Token {
        Token {
            kind: Kind::Number(f),
        }
    }
}
