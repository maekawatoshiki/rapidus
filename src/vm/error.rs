use super::super::builtins;
use crate::parser::ScriptInfo;
use crate::vm::frame::Frame;
use crate::vm::jsvalue::value::Value;
use crate::vm::vm::Factory;
use ansi_term::Colour;

#[derive(Clone, PartialEq, Debug)]
pub struct RuntimeError {
    /// A type of the error.
    kind: ErrorKind,
    /// Program counter where the error raised.
    inst_pc: usize,
    /// Function id where the error raised.
    func_id: usize,
    module_func_id: usize,
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
            ErrorKind::Exception(v, _) => v,
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

    pub fn show_error_message(&self, parse_info: &ScriptInfo) {
        match &self.kind {
            ErrorKind::Unknown => runtime_error("UnknownError"),
            ErrorKind::Unimplemented => runtime_error("Unimplemented feature"),
            ErrorKind::Reference(msg) => runtime_error(format!("ReferenceError: {}", msg)),
            ErrorKind::Type(msg) => runtime_error(format!("TypeError: {}", msg)),
            ErrorKind::General(msg) => runtime_error(format!("Error: {}", msg)),
            ErrorKind::Exception(ref val, ref node_pos) => {
                runtime_error("Uncaught Exception");
                if let Some(pos) = node_pos {
                    let (msg, _, line) = RuntimeError::get_code_around_err_point(parse_info, *pos);
                    println!("line: {}", line);
                    println!("{}", msg);
                }
                builtins::console::debug_print(val, false);
                println!();
            }
        }
    }

    pub fn get_code_around_err_point(info: &ScriptInfo, pos: usize) -> (String, usize, usize) {
        let code = info.code.as_bytes();
        let iter = info.pos_line_list.iter();
        let (start_pos, line) = iter.take_while(|x| x.0 <= pos).last().unwrap();

        let mut iter = info.pos_line_list.iter();
        let end_pos = match iter
            .find(|x| x.0 > pos)
            .unwrap_or(info.pos_line_list.last().unwrap())
            .0
        {
            x if x == 0 => 0,
            x => x - 1,
        };
        let surrounding_code = String::from_utf8(code[*start_pos..end_pos].to_vec())
            .unwrap()
            .to_string();
        let err_point = format!("{}{}", " ".repeat(pos - start_pos), "^",);
        (surrounding_code + "\n" + err_point.as_str(), pos, *line)
    }
}

pub fn runtime_error(msg: impl Into<String>) {
    eprintln!(
        "{}: {}",
        Colour::Red.bold().paint("runtime error"),
        msg.into(),
    );
}
