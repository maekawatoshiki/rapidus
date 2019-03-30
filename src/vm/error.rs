use ansi_term::Colour;
use gc::MemoryAllocator;
use vm::jsvalue::value::Value2;
use vm::value::Value;

#[derive(Clone, PartialEq, Debug)]
pub enum RuntimeError {
    Unknown,
    Type(String),
    Reference(String),
    General(String),
    Exception(Value),
    Exception2(Value2),
    Unimplemented,
}

impl RuntimeError {
    /// convert RuntimeError -> Value
    pub fn to_value(&self) -> Value {
        match self {
            RuntimeError::Exception(ref v) => v.clone(),
            RuntimeError::Exception2(ref _v) => Value::string("Value2".to_string()),
            RuntimeError::Type(ref s) => Value::string(s.clone()),
            RuntimeError::General(ref s) => Value::string(s.clone()),
            RuntimeError::Reference(ref s) => Value::string(s.clone()),
            RuntimeError::Unimplemented => Value::string("Unimplemented".to_string()),
            RuntimeError::Unknown => Value::string("Unknown".to_string()),
        }
    }

    /// convert RuntimeError -> Value2
    pub fn to_value2(self, memory_allocator: &mut MemoryAllocator) -> Value2 {
        match self {
            RuntimeError::Exception2(v) => v,
            RuntimeError::Type(s) => Value2::string(memory_allocator, s),
            RuntimeError::General(s) => Value2::string(memory_allocator, s),
            RuntimeError::Reference(s) => {
                Value2::string(memory_allocator, format!("Reference error: {}", s))
            }
            RuntimeError::Unimplemented => {
                Value2::string(memory_allocator, "Unimplemented".to_string())
            }
            RuntimeError::Unknown => Value2::string(memory_allocator, "Unknown".to_string()),
            _ => panic!(),
        }
    }

    pub fn show_error_message(&self) {
        match self {
            RuntimeError::Unknown => runtime_error("unknown error occurred"),
            RuntimeError::Unimplemented => runtime_error("unimplemented feature"),
            RuntimeError::Reference(msg) | RuntimeError::Type(msg) | RuntimeError::General(msg) => {
                runtime_error(msg.as_str())
            }
            RuntimeError::Exception2(val) => {
                runtime_error("Uncaught Exception");
                unsafe {
                    super::super::builtins::console::debug_print2(val, false);
                    libc::puts(b"\0".as_ptr() as *const i8);
                }
            }
            RuntimeError::Exception(val) => {
                runtime_error("Uncaught Exception");
                unsafe {
                    super::super::builtin::debug_print(&val, false);
                    libc::puts(b"\0".as_ptr() as *const i8);
                }
            }
        }
    }
}

pub fn runtime_error(msg: &str) {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
}
