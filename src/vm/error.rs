use super::super::builtins;
use crate::lexer;
use crate::vm::jsvalue::value::Value;
use crate::vm::vm::Factory;
use ansi_term::Colour;

#[derive(Clone, PartialEq, Debug)]
pub struct RuntimeError {
    /// A type of the error.
    kind: ErrorKind,
    /// Program counter where the error raised.
    pc: usize,
    /// Function id where the error raised.
    func_id: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ErrorKind {
    Unknown,
    Type(String),
    Reference(String),
    General(String),
    Exception(Value, Option<usize>),
    Unimplemented,
}

impl RuntimeError {
    pub fn new(kind: ErrorKind) -> RuntimeError {
        RuntimeError {
            kind,
            pc: 0,
            func_id: 0,
        }
    }

    pub fn general(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::General(msg.into()))
    }

    pub fn typeerr(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Type(msg.into()))
    }

    pub fn reference(msg: impl Into<String>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Reference(msg.into()))
    }

    pub fn exception(val: Value, node_pos: impl Into<Option<usize>>) -> RuntimeError {
        RuntimeError::new(ErrorKind::Exception(val, node_pos.into()))
    }

    pub fn unknown() -> RuntimeError {
        RuntimeError::new(ErrorKind::Unknown)
    }

    /// convert RuntimeError -> Value
    pub fn to_value(self, factory: &mut Factory) -> Value {
        match self.kind {
            ErrorKind::Exception(v, _) => v,
            ErrorKind::Type(s) => factory.string(format!("Type error: {}", s)),
            ErrorKind::General(s) => factory.string(s),
            ErrorKind::Reference(s) => factory.string(format!("Reference error: {}", s)),
            ErrorKind::Unimplemented => factory.string("Unimplemented"),
            ErrorKind::Unknown => factory.string("Unknown"),
        }
    }

    pub fn show_error_message(&self, lexer: Option<&lexer::Lexer>) {
        match &self.kind {
            ErrorKind::Unknown => runtime_error("UnknownError"),
            ErrorKind::Unimplemented => runtime_error("Unimplemented feature"),
            ErrorKind::Reference(msg) => runtime_error(format!("ReferenceError: {}", msg)),
            ErrorKind::Type(msg) => runtime_error(format!("TypeError: {}", msg)),
            ErrorKind::General(msg) => runtime_error(format!("Error: {}", msg)),
            ErrorKind::Exception(ref val, ref node_pos) => {
                runtime_error("Uncaught Exception");
                if let (Some(pos), Some(lexer)) = (node_pos, lexer) {
                    let (msg, _, line) = lexer.get_code_around_err_point(*pos);
                    println!("line: {}", line);
                    println!("{}", msg);
                }
                builtins::console::debug_print(val, false);
                println!();
            }
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
