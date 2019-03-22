// use super::super::frame::LexicalEnvironmentRef;
use super::value::*;
// use builtin::BuiltinFuncTy2;
// use bytecode_gen::ByteCode;

#[derive(Clone, Debug)]
pub struct ArrayObjectInfo {
    pub elems: Vec<Property2>,
}

impl ArrayObjectInfo {
    pub fn new(elems: Vec<Property2>) -> Self {
        ArrayObjectInfo { elems }
    }
}
