// use super::super::frame::LexicalEnvironmentRef;
use super::value::*;
// use builtin::BuiltinFuncTy2;
// use bytecode_gen::ByteCode;

#[derive(Clone, Debug)]
pub struct ArrayObjectInfo {
    pub elems: Vec<Property2>,
}
