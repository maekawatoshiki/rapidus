use super::value::*;
pub use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct ObjectInfo {
    pub kind: ObjectKind2,
    pub property: FxHashMap<String, Property2>,
}

#[derive(Clone, Debug)]
pub enum ObjectKind2 {
    Function(FunctionObjectInfo),
    Ordinary,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Property2 {
    pub val: Value2,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}
