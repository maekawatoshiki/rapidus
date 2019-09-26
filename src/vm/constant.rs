use crate::vm::jsvalue::value::Value;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    String(String),
    Value(Value),
    LexicalEnvironmentInfo {
        names: Vec<String>,
    },
    ObjectLiteralInfo {
        len: usize,
        special_properties: SpecialProperties,
    },
}

pub type SpecialProperties = FxHashMap<usize, SpecialPropertyKind>;

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum SpecialPropertyKind {
    Getter,
    Setter,
    Spread,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantTable {
    pub table: Vec<Constant>,
}

impl ConstantTable {
    pub fn new() -> Self {
        ConstantTable { table: vec![] }
    }

    pub fn add_string(&mut self, string: String) -> usize {
        for (i, constant) in self.table.iter().enumerate() {
            match constant {
                Constant::String(string_) if &string == string_ => return i,
                _ => {}
            }
        }

        let id = self.table.len();
        self.table.push(Constant::String(string));
        id
    }

    pub fn add_value(&mut self, value: Value) -> usize {
        for (i, constant) in self.table.iter().enumerate() {
            match constant {
                Constant::Value(value_) if &value == value_ => return i,
                _ => {}
            }
        }

        let id = self.table.len();
        self.table.push(Constant::Value(value));
        id
    }

    pub fn add_object_literal_info(
        &mut self,
        len: usize,
        special_properties: SpecialProperties,
    ) -> usize {
        for (i, constant) in self.table.iter().enumerate() {
            match constant {
                Constant::ObjectLiteralInfo {
                    len: len_,
                    special_properties: special_properties_,
                } if &special_properties == special_properties_ && &len == len_ => return i,
                _ => {}
            }
        }

        let id = self.table.len();
        self.table.push(Constant::ObjectLiteralInfo {
            len,
            special_properties,
        });
        id
    }

    pub fn add_lex_env_info(&mut self, names: Vec<String>) -> usize {
        let id = self.table.len();
        self.table.push(Constant::LexicalEnvironmentInfo { names });
        id
    }

    pub fn get(&self, id: usize) -> &Constant {
        &self.table[id]
    }

    pub fn get_mut(&mut self, id: usize) -> &mut Constant {
        &mut self.table[id]
    }
}

impl Constant {
    pub fn as_string(&self) -> &String {
        match self {
            Constant::String(string) => string,
            _ => panic!(),
        }
    }

    pub fn as_value(&self) -> &Value {
        match self {
            Constant::Value(value) => value,
            _ => panic!(),
        }
    }

    pub fn as_lex_env_info(&self) -> &Vec<String> {
        match self {
            Constant::LexicalEnvironmentInfo { names } => names,
            _ => panic!(),
        }
    }

    pub fn as_lex_env_info_mut(&mut self) -> &mut Vec<String> {
        match self {
            Constant::LexicalEnvironmentInfo { names } => names,
            _ => panic!(),
        }
    }

    pub fn as_object_literal_info(&self) -> (usize, SpecialProperties) {
        match self {
            Constant::ObjectLiteralInfo {
                len,
                special_properties,
            } => (*len, special_properties.clone()),
            _ => panic!(),
        }
    }
}
