use crate::vm::frame::Frame;
use crate::vm::jsvalue::value::Value;
use crate::vm::vm::Factory;
use ansi_term::Colour;

#[derive(Clone, PartialEq, Debug)]
pub struct RuntimeError {
    /// A type of the error.
    pub kind: ErrorKind,
    /// Program counter where the error raised.
    pub inst_pc: usize,
    /// Function id where the error raised.
    pub func_id: usize,
    pub module_func_id: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ErrorKind {
    Unknown,
    Type(String),
    Reference(String),
    General(String),
    Exception(Value),
    Unimplemented,
}

impl RuntimeError {
    pub fn new(kind: ErrorKind, frame: &Frame) -> RuntimeError {
        RuntimeError {
            kind,
            inst_pc: frame.current_inst_pc,
            func_id: frame.func_id,
            module_func_id: frame.module_func_id,
        }
    }

    fn default(kind: ErrorKind) -> RuntimeError {
        RuntimeError {
            kind,
            inst_pc: 0,
            func_id: 0,
            module_func_id: 0,
        }
    }

    pub fn typeerr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Type(msg.into()))
    }

    pub fn reference(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::default(ErrorKind::Reference(msg.into()))
    }

    pub fn error_add_info(mut self, frame: &Frame) -> RuntimeError {
        self.func_id = frame.func_id;
        self.module_func_id = frame.module_func_id;
        self.inst_pc = frame.pc;
        self
    }

    /// convert RuntimeError -> Value
    pub fn to_value(self, factory: &mut Factory) -> Value {
        match self.kind {
            ErrorKind::Exception(v) => v,
            ErrorKind::Type(s) => factory.error(format!("Type error: {}", s)),
            ErrorKind::General(s) => factory.error(format!("Error: {}", s)),
            ErrorKind::Reference(s) => factory.error(format!("Reference error: {}", s)),
            ErrorKind::Unimplemented => factory.error("Unimplemented"),
            ErrorKind::Unknown => factory.error("Unknown"),
        }
    }
}

pub fn runtime_error(msg: impl Into<String>) {
    eprintln!(
        "{}: {}",
        Colour::Red.bold().paint("runtime error"),
        msg.into(),
    );
}
