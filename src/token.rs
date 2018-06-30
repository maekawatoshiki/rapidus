use node::BinOp;

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
    Exp,
    Not,
    BitwiseNot,
    Shl,
    Shr,
    ZFShr, // Zero-Fill Right Shift
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    SEq, // Strict Equality
    Ne,
    SNe, // Strict Inequality
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

impl Token {
    pub fn is_the_keyword(&self, keyword: Keyword) -> bool {
        self.kind == Kind::Keyword(keyword)
    }

    pub fn is_the_symbol(&self, symbol: Symbol) -> bool {
        self.kind == Kind::Symbol(symbol)
    }
}

impl Symbol {
    pub fn as_binop(&self) -> Option<BinOp> {
        match self {
            Symbol::Add => Some(BinOp::Add),
            Symbol::Sub => Some(BinOp::Sub),
            Symbol::Asterisk => Some(BinOp::Mul),
            Symbol::Div => Some(BinOp::Div),
            Symbol::Mod => Some(BinOp::Rem),
            Symbol::And => Some(BinOp::And),
            Symbol::Or => Some(BinOp::Or),
            Symbol::Xor => Some(BinOp::Xor),
            Symbol::LAnd => Some(BinOp::LAnd),
            Symbol::LOr => Some(BinOp::LOr),
            Symbol::Eq => Some(BinOp::Eq),
            Symbol::Ne => Some(BinOp::Ne),
            Symbol::SEq => Some(BinOp::SEq),
            Symbol::SNe => Some(BinOp::SNe),
            Symbol::Lt => Some(BinOp::Lt),
            Symbol::Gt => Some(BinOp::Gt),
            Symbol::Le => Some(BinOp::Le),
            Symbol::Ge => Some(BinOp::Ge),
            Symbol::Shl => Some(BinOp::Shl),
            Symbol::Shr => Some(BinOp::Shr),
            Symbol::ZFShr => Some(BinOp::ZFShr),
            Symbol::Comma => Some(BinOp::Comma),
            Symbol::Assign => Some(BinOp::Assign),
            _ => None,
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
