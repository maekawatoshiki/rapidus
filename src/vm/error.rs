use ansi_term::Colour;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    Unknown,
    Type(String),
    Reference(String),
    Unimplemented,
}

pub fn runtime_error(msg: &str) {
    eprintln!("{}: {}", Colour::Red.bold().paint("runtime error"), msg,);
}
