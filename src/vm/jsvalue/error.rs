use crate::vm::jsvalue::value::*;

#[derive(Clone, Debug)]
pub struct ErrorObjectInfo {
    pub elems: Vec<Property>,
}
