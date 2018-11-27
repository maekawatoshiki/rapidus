// TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
#[derive(Clone, Debug, PartialEq)]
pub struct FormalParameter {
    pub name: String,
    pub init: Option<Node>,
    pub is_rest_param: bool,
}

pub type FormalParameters = Vec<FormalParameter>;

impl FormalParameter {
    pub fn new(name: String, init: Option<Node>, is_rest_param: bool) -> FormalParameter {
        FormalParameter {
            name: name,
            init: init,
            is_rest_param: is_rest_param,
        }
    }
}

// TODO: Support all features: https://tc39.github.io/ecma262/#prod-PropertyDefinition
#[derive(Clone, Debug, PartialEq)]
pub enum PropertyDefinition {
    IdentifierReference(String),
    Property(String, Node),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub base: NodeBase,
    pub pos: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeBase {
    StatementList(Vec<Node>),
    FunctionDecl(String, FormalParameters, Box<Node>), // name, params, body
    FunctionExpr(Option<String>, FormalParameters, Box<Node>), // Name, params, body
    VarDecl(String, Option<Box<Node>>),
    Member(Box<Node>, String),
    Index(Box<Node>, Box<Node>),
    New(Box<Node>),
    Call(Box<Node>, Vec<Node>),
    If(Box<Node>, Box<Node>, Box<Node>), // Cond, Then, Else
    While(Box<Node>, Box<Node>),         // Cond, Body
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // Init, Cond, Step, Body
    Assign(Box<Node>, Box<Node>),
    UnaryOp(Box<Node>, UnaryOp),
    BinaryOp(Box<Node>, Box<Node>, BinOp),
    TernaryOp(Box<Node>, Box<Node>, Box<Node>),
    Return(Option<Box<Node>>),
    Break,
    Continue,
    Array(Vec<Node>),
    Object(Vec<PropertyDefinition>),
    Identifier(String),
    This,
    Arguments,
    Undefined,
    String(String),
    Boolean(bool),
    Number(f64),
    Nope,
}

impl Node {
    pub fn new(base: NodeBase, pos: usize) -> Node {
        Node {
            base: base,
            pos: pos,
        }
    }
}

impl NodeBase {
    pub fn fold_num_consts(&self) -> Option<NodeBase> {
        // TODO: Support If, UnaryOp, TernaryOp.
        match self {
            // NodeBase::StatementList(Vec<Node>),
            // NodeBase::FunctionDecl(String, FormalParameters, Box<Node>), // name, params, body
            // NodeBase::FunctionExpr(Option<String>, FormalParameters, Box<Node>), // Name, params, body
            // NodeBase::VarDecl(String, Option<Box<Node>>),
            // NodeBase::Member(Box<Node>, String),
            // NodeBase::Index(Box<Node>, Box<Node>),
            // NodeBase::New(Box<Node>),
            // NodeBase::Call(Box<Node>, Vec<Node>),
            // NodeBase::If(Box<Node>, Box<Node>, Box<Node>), // Cond, Then, Else
            // NodeBase::While(Box<Node>, Box<Node>),         // Cond, Body
            // NodeBase::For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // Init, Cond, Step, Body
            // NodeBase::Assign(Box<Node>, Box<Node>),
            // NodeBase::UnaryOp(Box<Node>, UnaryOp),
            NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                let lhs = (*lhs).base.fold_num_consts()?;
                let rhs = (*rhs).base.fold_num_consts()?;

                match (lhs, rhs) {
                    (NodeBase::Number(l), NodeBase::Number(r)) => match op {
                        BinOp::Add => Some(NodeBase::Number(l + r)),
                        BinOp::Sub => Some(NodeBase::Number(l - r)),
                        BinOp::Mul => Some(NodeBase::Number(l * r)),
                        BinOp::Div => Some(NodeBase::Number(l / r)),
                        BinOp::Rem => Some(NodeBase::Number(((l as u64) % (r as u64)) as f64)),
                        BinOp::Exp => Some(NodeBase::Number(l.powf(r))),
                        BinOp::And => Some(NodeBase::Number(((l as u64) & (r as u64)) as f64)),
                        BinOp::Or => Some(NodeBase::Number(((l as u64) | (r as u64)) as f64)),
                        BinOp::Xor => Some(NodeBase::Number(((l as u64) ^ (r as u64)) as f64)),
                        BinOp::LAnd => Some(NodeBase::Boolean((l > 0.0) && (r > 0.0))),
                        BinOp::LOr => Some(NodeBase::Boolean((l > 0.0) || (r > 0.0))),
                        BinOp::Eq => Some(NodeBase::Boolean(l == r)),
                        BinOp::Ne => Some(NodeBase::Boolean(l != r)),
                        BinOp::SEq => Some(NodeBase::Boolean(l == r)),
                        BinOp::SNe => Some(NodeBase::Boolean(l != r)),
                        BinOp::Lt => Some(NodeBase::Boolean(l < r)),
                        BinOp::Gt => Some(NodeBase::Boolean(l > r)),
                        BinOp::Le => Some(NodeBase::Boolean(l <= r)),
                        BinOp::Ge => Some(NodeBase::Boolean(l >= r)),
                        BinOp::Shl => Some(NodeBase::Number(
                            ((l as i64 as i32) << r as i64 as i32) as f64,
                        )),
                        BinOp::Shr => Some(NodeBase::Number(
                            ((l as i64 as i32) >> (r as i64 as i32)) as f64,
                        )),
                        BinOp::ZFShr => Some(NodeBase::Number(
                            ((l as u64 as u32) >> (r as u64 as u32)) as f64,
                        )),
                        _ => None,
                    },
                    (NodeBase::String(l), NodeBase::String(r)) => match op {
                        BinOp::Add => Some(NodeBase::String(l + r.as_str())),
                        _ => None,
                    },
                    _ => None,
                }
            }
            // NodeBase::TernaryOp(Box<Node>, Box<Node>, Box<Node>),
            // NodeBase::Return(Option<Box<Node>>),
            // NodeBase::Break,
            // NodeBase::Continue,
            // NodeBase::Array(Vec<Node>),
            // NodeBase::Object(Vec<PropertyDefinition>),
            // NodeBase::Identifier(String),
            // NodeBase::This,
            // NodeBase::Arguments,
            NodeBase::String(s) => Some(NodeBase::String(s.clone())),
            NodeBase::Boolean(b) => Some(NodeBase::Boolean(*b)),
            NodeBase::Number(n) => Some(NodeBase::Number(*n)),
            // NodeBase::Nope,
            // NodeBase::SetCurCallObj(String),
            _ => None,
        }
    }
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
