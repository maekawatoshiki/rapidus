use vm::constant::ConstantTable;

#[derive(Debug)]
pub struct CodeGenerator<'a> {
    constant_table: &'a mut ConstantTable,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(constant_table: &'a mut ConstantTable) -> Self {
        CodeGenerator { constant_table }
    }
}
