use ansi_term::Colour;
use vm::value::Value;

#[derive(Debug, Clone, PartialEq)]
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
}

pub fn runtime_error(msg: &str) {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
}
