use super::super::builtins;
use crate::lexer;
use crate::vm::jsvalue::value::Value;
use crate::vm::vm::Factory;
use ansi_term::Colour;

#[derive(Clone, PartialEq, Debug)]
pub enum RuntimeError {
    Unknown,
    Type(String),
    Reference(String),
    General(String),
    Exception2(Value, Option<usize>),
    Unimplemented,
}

impl RuntimeError {
    /// convert RuntimeError -> Value
    pub fn to_value(self, factory: &mut Factory) -> Value {
        match self {
            RuntimeError::Exception2(v, _) => v,
            RuntimeError::Type(s) => factory.string(s),
            RuntimeError::General(s) => factory.string(s),
            RuntimeError::Reference(s) => factory.string(format!("Reference error: {}", s)),
            RuntimeError::Unimplemented => factory.string("Unimplemented"),
            RuntimeError::Unknown => factory.string("Unknown"),
        }
    }

    pub fn show_error_message(&self, lexer: Option<&lexer::Lexer>) {
        match self {
            RuntimeError::Unknown => runtime_error("unknown error occurred"),
            RuntimeError::Unimplemented => runtime_error("unimplemented feature"),
            RuntimeError::Reference(msg) | RuntimeError::Type(msg) | RuntimeError::General(msg) => {
                runtime_error(msg.as_str())
            }
            RuntimeError::Exception2(val, node_pos) => {
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

pub fn runtime_error(msg: &str) {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
}
