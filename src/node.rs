use std::boxed::Box;
use std::collections::HashSet;

// TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
#[derive(Clone, Debug, PartialEq)]
pub struct FormalParameter {
    pub name: String,
    pub init: Option<Node>,
}

pub type FormalParameters = Vec<FormalParameter>;

impl FormalParameter {
    pub fn new(name: String, init: Option<Node>) -> FormalParameter {
        FormalParameter {
            name: name,
            init: init,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    StatementList(Vec<Node>),
    FunctionDecl(Option<String>, bool, HashSet<String>, FormalParameters, Box<Node>), // Name, Use 'this', fv, params, body
    VarDecl(String, Option<Box<Node>>),
    Member(Box<Node>, String),
    New(Box<Node>),
    Call(Box<Node>, Vec<Node>),
    If(Box<Node>, Box<Node>, Box<Node>), // Cond, Then, Else
    While(Box<Node>, Box<Node>),         // Cond, Body
    Assign(Box<Node>, Box<Node>),
    UnaryOp(Box<Node>, UnaryOp),
    BinaryOp(Box<Node>, Box<Node>, BinOp),
    TernaryOp(Box<Node>, Box<Node>, Box<Node>),
    Return(Option<Box<Node>>),
    Identifier(String),
    This,
    String(String),
    Boolean(bool),
    Number(f64),
    Nope,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Delete,
    Void,
    Typeof,
    Plus,
    Minus,
    BitwiseNot,
    Not,
    PrInc, // Prefix
    PrDec,
    PoInc, // Postfix
    PoDec,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,
    And,
    Or,
    Xor,
    LAnd,
    LOr,
    Eq,
    Ne,
    SEq, // Strict Eq
    SNe, // Strict Ne
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    ZFShr,
    Comma,
    Assign,
}
