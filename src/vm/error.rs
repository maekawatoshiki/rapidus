use ansi_term::Colour;
use vm::value::Value;

#[derive(Clone, PartialEq, Debug)]
pub enum RuntimeError {
    Unknown,
    Type(String),
    Reference(String),
    General(String),
    Exception(Value),
    Unimplemented,
}

impl RuntimeError {
    /// convert RuntimeError -> Value.
    pub fn to_value(&self) -> Value {
        match self {
            RuntimeError::Exception(ref v) => v.clone(),
            RuntimeError::Type(ref s) => Value::string(s.clone()),
            RuntimeError::General(ref s) => Value::string(s.clone()),
            RuntimeError::Reference(ref s) => Value::string(s.clone()),
            RuntimeError::Unimplemented => Value::string("Unimplemented".to_string()),
            RuntimeError::Unknown => Value::string("Unknown".to_string()),
        }
    }

    pub fn show_error_message(&self) {
        match self {
            RuntimeError::Unknown => runtime_error("unknown error occurred"),
            RuntimeError::Unimplemented => runtime_error("unimplemented feature"),
            RuntimeError::Reference(msg) | RuntimeError::Type(msg) | RuntimeError::General(msg) => {
                runtime_error(msg.as_str())
            }
            RuntimeError::Exception(val) => {
                runtime_error("Uncaught Exception");
                unsafe {
                    super::super::builtin::debug_print(&val, true);
                    libc::puts(b"\0".as_ptr() as *const i8);
                }
            }
        }
    }
}

pub fn runtime_error(msg: &str) {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
}
