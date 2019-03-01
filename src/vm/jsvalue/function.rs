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
    pub code: ByteCode,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: String,
    pub is_rest_param: bool,
}
