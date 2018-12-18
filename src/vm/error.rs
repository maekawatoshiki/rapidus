use ansi_term::Colour;
use vm::value;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    Unknown,
    Type(String),
    Reference(String),
    General(String),
    Exception(value::Value),
    Unimplemented,
    Return
}

pub fn runtime_error(msg: &str) {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
}
