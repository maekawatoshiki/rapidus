use super::super::frame::LexicalEnvironmentRef;
use super::value::*;
use builtin::BuiltinFuncTy2;
use bytecode_gen::ByteCode;

#[derive(Clone, Debug)]
pub struct FunctionObjectInfo {
    pub id: usize,
    pub name: Option<String>,
    pub kind: FunctionObjectKind,
}

#[derive(Clone)]
pub enum FunctionObjectKind {
    User(UserFunctionInfo),
    Builtin(BuiltinFuncTy2),
}

#[derive(Clone, Debug)]
pub struct UserFunctionInfo {
    pub params: Vec<FunctionParameter>,
    pub var_names: Vec<String>,
    pub lex_names: Vec<String>,
    pub func_decls: Vec<Value2>,
    pub constructor: bool,
    pub bound_variables: usize,
    pub code: ByteCode,
    pub exception_table: Vec<Exception>,
    pub outer: Option<LexicalEnvironmentRef>,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: String,
    pub is_rest_param: bool,
    pub bound: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct Exception {
    /// Throws may happen in bytecode's range of [start, end)
    pub start: usize,
    pub end: usize,
    /// Kind of throw's destination
    pub dst_kind: DestinationKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DestinationKind {
    Catch,
    Finally,
}

impl FunctionObjectInfo {
    pub fn set_outer_environment(&mut self, outer_env: LexicalEnvironmentRef) {
        match self.kind {
            FunctionObjectKind::User(UserFunctionInfo { ref mut outer, .. }) => {
                *outer = Some(outer_env)
            }
            _ => {}
        }
    }
}

impl ::std::fmt::Debug for FunctionObjectKind {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FunctionObjectKind::User(user_func) => format!("{:?}", user_func),
                FunctionObjectKind::Builtin(_) => "[BuiltinFunction]".to_string(),
            }
        )
    }
}
