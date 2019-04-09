use super::super::builtins;
use crate::lexer;
use ansi_term::Colour;
use gc::MemoryAllocator;
use vm::jsvalue::value::Value;

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
    pub fn to_value2(self, memory_allocator: &mut MemoryAllocator) -> Value {
        match self {
            RuntimeError::Exception2(v, _) => v,
            RuntimeError::Type(s) => Value::string(memory_allocator, s),
            RuntimeError::General(s) => Value::string(memory_allocator, s),
            RuntimeError::Reference(s) => {
                Value::string(memory_allocator, format!("Reference error: {}", s))
            }
            RuntimeError::Unimplemented => {
                Value::string(memory_allocator, "Unimplemented".to_string())
            }
            RuntimeError::Unknown => Value::string(memory_allocator, "Unknown".to_string()),
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
