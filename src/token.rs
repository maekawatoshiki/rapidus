#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: Kind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    Keyword(Keyword),
    Identifier(String),
    Number(f64),
    String(String),
    Symbol(Symbol),
    LineTerminator,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    Break,
    Case,
    Catch,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Finally,
    For,
    Function,
    If,
    In,
    Instanceof,
    New,
    Return,
    Switch,
    This,
    Throw,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
}

#[derive(Clone, Debug, PartialEq)]
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

    pub fn new_keyword(keyword: Keyword) -> Token {
        Token {
            kind: Kind::Keyword(keyword),
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

pub fn convert_reserved_keyword(keyword: &str) -> Option<Keyword> {
    match keyword {
        "break" => Some(Keyword::Break),
        "case" => Some(Keyword::Case),
        "catch" => Some(Keyword::Catch),
        "continue" => Some(Keyword::Continue),
        "debugger" => Some(Keyword::Debugger),
        "default" => Some(Keyword::Default),
        "delete" => Some(Keyword::Delete),
        "do" => Some(Keyword::Do),
        "else" => Some(Keyword::Else),
        "finally" => Some(Keyword::Finally),
        "for" => Some(Keyword::For),
        "function" => Some(Keyword::Function),
        "if" => Some(Keyword::If),
        "in" => Some(Keyword::In),
        "instanceof" => Some(Keyword::Instanceof),
        "new" => Some(Keyword::New),
        "return" => Some(Keyword::Return),
        "switch" => Some(Keyword::Switch),
        "this" => Some(Keyword::This),
        "throw" => Some(Keyword::Throw),
        "try" => Some(Keyword::Try),
        "typeof" => Some(Keyword::Typeof),
        "var" => Some(Keyword::Var),
        "void" => Some(Keyword::Void),
        "while" => Some(Keyword::While),
        "with" => Some(Keyword::With),
        _ => None,
    }
}
