use crate::node::BinOp;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub pos: usize,
    /// char position of previous token.
    /// lexer generate this after tokenizing.
    pub prev_pos: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    Keyword(Keyword),
    Identifier(String),
    Number(f64),
    String(String),
    Symbol(Symbol),
    LineTerminator,
    EOF,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Keyword {
    Abstract,
    Arguments,
    Break,
    Case,
    Catch,
    Const,
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
    Let,
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
    Rest,
    FatArrow,
}

impl Token {
    pub fn new_number(f: f64, pos: usize) -> Token {
        Token {
            kind: Kind::Number(f),
            pos: pos,
            prev_pos: 0,
        }
    }

    pub fn new_identifier(ident: String, pos: usize) -> Token {
        Token {
            kind: Kind::Identifier(ident),
            pos: pos,
            prev_pos: 0,
        }
    }

    pub fn new_keyword(keyword: Keyword, pos: usize) -> Token {
        Token {
            kind: Kind::Keyword(keyword),
            pos: pos,
            prev_pos: 0,
        }
    }

    pub fn new_string(s: String, pos: usize) -> Token {
        Token {
            kind: Kind::String(s),
            pos: pos,
            prev_pos: 0,
        }
    }

    pub fn new_symbol(symbol: Symbol, pos: usize) -> Token {
        Token {
            kind: Kind::Symbol(symbol),
            pos: pos,
            prev_pos: 0,
        }
    }

    pub fn new_line_terminator(pos: usize) -> Token {
        Token {
            kind: Kind::LineTerminator,
            pos: pos,
            prev_pos: 0,
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

    pub fn is_identifier(&self) -> bool {
        match self.kind {
            Kind::Identifier(_) => true,
            _ => false,
        }
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

impl Keyword {
    pub fn to_str(self) -> &'static str {
        match self {
            Keyword::Abstract => "abstract",
            Keyword::Arguments => "arguments",
            Keyword::Break => "break",
            Keyword::Case => "case",
            Keyword::Catch => "catch",
            Keyword::Continue => "continue",
            Keyword::Const => "const",
            Keyword::Debugger => "debugger",
            Keyword::Default => "default",
            Keyword::Delete => "delete",
            Keyword::Do => "do",
            Keyword::Else => "else",
            Keyword::Finally => "finally",
            Keyword::For => "for",
            Keyword::Function => "function",
            Keyword::If => "if",
            Keyword::In => "in",
            Keyword::Instanceof => "instanceof",
            Keyword::Let => "let",
            Keyword::New => "new",
            Keyword::Return => "return",
            Keyword::Switch => "switch",
            Keyword::This => "this",
            Keyword::Throw => "throw",
            Keyword::Try => "try",
            Keyword::Typeof => "typeof",
            Keyword::Var => "var",
            Keyword::Void => "void",
            Keyword::While => "while",
            Keyword::With => "with",
        }
    }
}

pub fn convert_reserved_keyword(keyword: &str) -> Option<Keyword> {
    match keyword {
        "abstract" => Some(Keyword::Abstract),
        "arguments" => Some(Keyword::Arguments),
        "break" => Some(Keyword::Break),
        "case" => Some(Keyword::Case),
        "catch" => Some(Keyword::Catch),
        "continue" => Some(Keyword::Continue),
        "const" => Some(Keyword::Const),
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
        "let" => Some(Keyword::Let),
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

pub fn get_string_for_symbol(symbol: Symbol) -> String {
    match symbol {
        Symbol::OpeningParen => "(".to_string(),
        Symbol::ClosingParen => ")".to_string(),
        Symbol::OpeningBrace => "{".to_string(),
        Symbol::ClosingBrace => "}".to_string(),
        Symbol::OpeningBoxBracket => "[".to_string(),
        Symbol::ClosingBoxBracket => "]".to_string(),
        Symbol::Comma => ",".to_string(),
        Symbol::Semicolon => ";".to_string(),
        Symbol::Colon => ":".to_string(),
        Symbol::Point => ".".to_string(),
        Symbol::Arrow => "=>".to_string(),
        Symbol::Inc => "++".to_string(),
        Symbol::Dec => "--".to_string(),
        Symbol::Add => "+".to_string(),
        Symbol::Sub => "-".to_string(),
        Symbol::Asterisk => "*".to_string(),
        _ => "???".to_string(),
    }
}
