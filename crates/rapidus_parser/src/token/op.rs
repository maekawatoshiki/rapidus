#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    LShift,
    RShift,
    URShift,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    NullishCoalescing,
}
