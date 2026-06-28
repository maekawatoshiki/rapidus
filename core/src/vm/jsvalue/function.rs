use crate::builtins::BuiltinFuncTy;
use crate::bytecode_gen::{ByteCode, VMInst};
use crate::vm::exec_context::LexicalEnvironmentRef;
use crate::vm::factory::{Factory, FunctionId};

#[derive(Clone, Debug)]
pub struct FunctionObjectInfo {
    pub name: Option<String>,
    pub super_constructor: Option<crate::vm::jsvalue::value::Value>,
    pub kind: FunctionObjectKind,
}

#[derive(Clone)]
pub enum FunctionObjectKind {
    User {
        /// Internal slot \[\[Environment\]\]
        outer_env: Option<LexicalEnvironmentRef>,
        info: FuncInfoRef,
    },
    Builtin(BuiltinFuncTy),
}

#[derive(Clone, Debug)]
pub struct UserFunctionInfo {
    pub func_name: Option<String>,

    /// Unique id for many purposes
    pub func_id: FunctionId,

    /// Module id
    pub module_func_id: FunctionId,

    /// Internal slot \[\[FormalParameters\]\]
    pub params: Vec<FunctionParameter>,

    /// Function `length` property.
    pub length: usize,

    /// Varaible declared names
    pub var_names: Vec<String>,

    /// Lexically declared names
    pub lex_names: Vec<String>,

    /// Lexically declared immutable names
    pub const_names: Vec<String>,

    /// Declared functions to initialize
    pub func_decls: Vec<FuncInfoRef>,

    /// Bytecode to execute
    pub code: ByteCode,

    /// Bytecode offset immediately after parameter initializers.
    pub parameter_init_len: usize,

    /// Exception table
    pub exception_table: Vec<Exception>,

    /// Represent if constructible or not
    pub constructible: bool,

    /// Internal slot [[GeneratorKind]] for synchronous generators.
    pub generator: bool,

    /// True for AsyncFunction objects.
    pub async_function: bool,

    /// Internal slot \[\[ThisMode\]\]
    pub this_mode: ThisMode,
}

#[derive(Clone, Debug, Copy)]
pub struct FuncInfoRef(*mut UserFunctionInfo);

impl UserFunctionInfo {
    pub fn as_ref(&mut self) -> FuncInfoRef {
        FuncInfoRef::new(&mut *self as *mut UserFunctionInfo)
    }
}

impl FuncInfoRef {
    pub fn as_ptr(self) -> *mut UserFunctionInfo {
        self.0
    }

    pub fn new(info: *mut UserFunctionInfo) -> FuncInfoRef {
        FuncInfoRef(info)
    }

    pub fn default() -> FuncInfoRef {
        FuncInfoRef(std::ptr::null_mut() as *mut UserFunctionInfo)
    }
}

impl std::ops::Deref for FuncInfoRef {
    type Target = UserFunctionInfo;

    fn deref(&self) -> &UserFunctionInfo {
        //println!("deref");
        let refs = unsafe { &*self.as_ptr() };
        //println!("derefed {}", refs.func_id.0);
        refs
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ThisMode {
    Lexical,
    Global,
    Strict,
    Derived,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: String,
    pub rest_param: bool,
    pub has_initializer: bool,
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
            func_name: None,
            func_id: factory.new_func_id(),
            module_func_id,
            params: vec![],
            length: 0,
            var_names: vec![],
            lex_names: vec![],
            const_names: vec![],
            func_decls: vec![],
            constructible: false,
            generator: false,
            async_function: false,
            this_mode: ThisMode::Global,
            code: vec![VMInst::PUSH_UNDEFINED, VMInst::RETURN],
            parameter_init_len: 0,
            exception_table: vec![],
        }
    }

    pub fn default() -> Self {
        UserFunctionInfo {
            func_name: None,
            func_id: FunctionId::default(),
            module_func_id: FunctionId::default(),
            params: vec![],
            length: 0,
            var_names: vec![],
            lex_names: vec![],
            const_names: vec![],
            func_decls: vec![],
            constructible: false,
            generator: false,
            async_function: false,
            this_mode: ThisMode::Global,
            code: vec![VMInst::PUSH_UNDEFINED, VMInst::RETURN],
            parameter_init_len: 0,
            exception_table: vec![],
        }
    }
}

impl FunctionObjectInfo {
    pub fn set_outer_environment(&mut self, env: LexicalEnvironmentRef) {
        match self.kind {
            FunctionObjectKind::User {
                ref mut outer_env, ..
            } => *outer_env = Some(env),
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
                FunctionObjectKind::User { info, .. } => format!("{:?}", info),
                FunctionObjectKind::Builtin(_) => "[BuiltinFunction]".to_string(),
            }
        )
    }
}
