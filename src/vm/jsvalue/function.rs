use super::value::*;
use crate::builtin::BuiltinFuncTy;
use crate::bytecode_gen::ByteCode;
use crate::vm::exec_context::LexicalEnvironmentRef;
use crate::vm::factory::{Factory, FunctionId};

#[derive(Clone, Debug)]
pub struct FunctionObjectInfo {
    pub name: Option<String>,
    pub kind: FunctionObjectKind,
}

#[derive(Clone)]
pub enum FunctionObjectKind {
    User(UserFunctionInfo),
    Builtin(BuiltinFuncTy),
}

#[derive(Clone, Debug)]
pub struct UserFunctionInfo {
    /// Unique id for many purposes
    pub id: FunctionId,

    /// Module id
    pub module_func_id: FunctionId,

    /// Internal slot \[\[FormalParameters\]\]
    pub params: Vec<FunctionParameter>,

    /// Varaible declared names
    pub var_names: Vec<String>,

    /// Lexically declared names
    pub lex_names: Vec<String>,

    /// Declared functions to initialize
    pub func_decls: Vec<Value>,

    /// Bytecode to execute
    pub code: ByteCode,

    /// Exception table
    pub exception_table: Vec<Exception>,

    /// Represent if constructible or not
    pub constructible: bool,

    /// Internal slot \[\[ThisMode\]\]
    pub this_mode: ThisMode,

    /// Internal slot \[\[Environment\]\]
    // TODO: Should rename 'outer' to 'environment'?
    pub outer: Option<LexicalEnvironmentRef>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ThisMode {
    Lexical,
    Global,
    Strict,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: String,
    pub rest_param: bool,
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

impl UserFunctionInfo {
    pub fn new(factory: &mut Factory, module_func_id: FunctionId) -> Self {
        UserFunctionInfo {
            id: factory.new_func_id(),
            module_func_id,
            params: vec![],
            var_names: vec![],
            lex_names: vec![],
            func_decls: vec![],
            constructible: false,
            this_mode: ThisMode::Global,
            code: vec![0x0c, 0x28], // [PUSH_UNDEFINED][RETURN]
            exception_table: vec![],
            outer: None,
        }
    }

    pub fn default() -> Self {
        UserFunctionInfo {
            id: FunctionId::default(),
            module_func_id: FunctionId::default(),
            params: vec![],
            var_names: vec![],
            lex_names: vec![],
            func_decls: vec![],
            constructible: false,
            this_mode: ThisMode::Global,
            code: vec![0x0c, 0x28], // [PUSH_UNDEFINED][RETURN]
            exception_table: vec![],
            outer: None,
        }
    }
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
