use crate::vm::exec_context::ExecContext;
use crate::vm::jsvalue::value::Value;
use crate::vm::vm::{Factory, FunctionId};
use ansi_term::Colour;

#[derive(Clone, PartialEq, Debug)]
pub struct RuntimeError {
    /// A type of the error.
    pub kind: ErrorKind,
    /// Program counter where the error raised.
    pub inst_pc: usize,
    /// Function id where the error raised.
    pub func_id: FunctionId,
    /// Module function id where the error raised.
    pub module_func_id: FunctionId,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ErrorKind {
    Unknown,
    Type(String),
    Range(String),
    Syntax(String),
    Uri(String),
    Reference(String),
    General(String),
    Exception(Value),
    Unimplemented,
}

impl RuntimeError {
    pub fn new(kind: ErrorKind, context: &ExecContext) -> RuntimeError {
        RuntimeError {
            kind,
            inst_pc: context.current_inst_pc,
            func_id: context.func_ref.func_id,
            module_func_id: context.func_ref.module_func_id,
        }
    }

    fn default(kind: ErrorKind) -> RuntimeError {
        RuntimeError {
            kind,
            inst_pc: 0,
            func_id: FunctionId::default(),
            module_func_id: FunctionId::default(),
        }
    }

    pub fn typeerr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Type(msg.into()))
    }

    pub fn rangeerr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Range(msg.into()))
    }

    pub fn syntaxerr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Syntax(msg.into()))
    }

    pub fn urierr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Uri(msg.into()))
    }

    pub fn reference(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Reference(msg.into()))
    }

    pub fn error_add_info(mut self, context: &ExecContext) -> RuntimeError {
        self.func_id = context.func_ref.func_id;
        self.module_func_id = context.func_ref.module_func_id;
        self.inst_pc = context.pc;
        self
    }

    /// convert RuntimeError -> Value
    pub fn to_value(self, factory: &mut Factory) -> Value {
        match self.kind {
            ErrorKind::Exception(v) => v,
            ErrorKind::Type(s) => factory.native_error("TypeError", s),
            ErrorKind::Range(s) => factory.native_error("RangeError", s),
            ErrorKind::Syntax(s) => factory.native_error("SyntaxError", s),
            ErrorKind::Uri(s) => factory.native_error("URIError", s),
            ErrorKind::General(s) => factory.error(format!("Error: {}", s)),
            ErrorKind::Reference(s) => factory.native_error("ReferenceError", s),
            ErrorKind::Unimplemented => factory.error("Unimplemented"),
            ErrorKind::Unknown => factory.error("Unknown"),
        }
    }
}

pub fn runtime_error(msg: impl Into<String>) {
    eprintln!(
        "{}: {}",
        Colour::Red.bold().paint("Runtime error"),
        msg.into(),
    );
}
