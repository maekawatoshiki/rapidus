use vm::value::Value2;

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    String(String),
    Value(Value2),
    LexicalEnvironmentInfo,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantTable {
    table: Vec<Constant>,
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

    pub fn add_value(&mut self, value: Value2) -> usize {
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
}
