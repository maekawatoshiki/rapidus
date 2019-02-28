#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    String(String),
    Value,
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

    pub fn add_string(&mut self, string: String) {
        self.table.push(Constant::String(string));
    }

    // pub fn add_value(&mut self,value:V) {
    //     self.table.push(Constant::String(string));
    // }
}
