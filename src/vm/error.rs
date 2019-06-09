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

    fn new_(kind: ErrorKind) -> RuntimeError {
        RuntimeError {
            kind,
            inst_pc: 0,
            func_id: 0,
            module_func_id: 0,
        }
    }

    pub fn typeerr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new_(ErrorKind::Type(msg.into()))
    }

    pub fn reference(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new_(ErrorKind::Reference(msg.into()))
    }

    pub fn error_add_info(mut self, frame: &Frame) -> RuntimeError {
        self.func_id = frame.func_id;
        self.module_func_id = frame.module_func_id;
        self.inst_pc = frame.pc;
        self
    }

    /// convert RuntimeError -> Value
    pub fn to_value(self, factory: &mut Factory) -> Value {
        let err_obj = make_normal_object!(factory);
        match self.kind {
            ErrorKind::Exception(v) => v,
            ErrorKind::Type(s) => {
                let v = factory.string(format!("Type error: {}", s));
                err_obj.set_property_by_string_key("message", v);
                err_obj
            }
            ErrorKind::General(s) => {
                let v = factory.string(format!("Error: {}", s));
                err_obj.set_property_by_string_key("message", v);
                err_obj
            }
            ErrorKind::Reference(s) => {
                let v = factory.string(format!("Reference error: {}", s));
                err_obj.set_property_by_string_key("message", v);
                err_obj
            }
            ErrorKind::Unimplemented => factory.string("Unimplemented"),
            ErrorKind::Unknown => factory.string("Unknown"),
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
