#[derive(Clone, Debug)]
pub struct Token {
    pub kind: Kind,
}

#[derive(Clone, Debug)]
pub enum Kind {
    Keyword(Keyword),
    Identifier(String),
    Number(f64),
    String(String),
    Symbol(Symbol),
    LineTerminator,
}

#[derive(Clone, Debug)]
pub enum Keyword {
}

#[derive(PartialEq, Debug, Clone)]
pub enum Symbol {
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    OpeningBoxBracket,
    ClosingBoxBracket,
    Comma,
    Semicolon,
    Colon,
    Point,
    Arrow,
    Inc,
    Dec,
    Add,
    Sub,
    Asterisk,
    Div,
    Mod,
    Not,
    BitwiseNot,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    And,
    Or,
    Xor,
    LAnd,
    LOr,
    Question,
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignShl,
    AssignShr,
    AssignAnd,
    AssignOr,
    AssignXor,
    AssignLAnd,
    AssignLOr,
    Hash,
}

impl Token {
    pub fn new_number(f: f64) -> Token {
        Token {
            kind: Kind::Number(f),
        }
    }

    pub fn new_identifier(ident: String) -> Token {
        Token {
            kind: Kind::Identifier(ident),
        }
    }

    pub fn new_string(s: String) -> Token {
        Token {
            kind: Kind::String(s),
        }
    }

    pub fn new_symbol(symbol: Symbol) -> Token {
        Token {
            kind: Kind::Symbol(symbol),
        }
    }

    pub fn new_line_terminator() -> Token {
        Token {
            kind: Kind::LineTerminator,
        }
    }
}
