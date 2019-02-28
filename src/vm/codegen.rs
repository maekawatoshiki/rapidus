use bytecode_gen::{ByteCode, ByteCodeGen};
use node::{Node, NodeBase};
use vm::constant::ConstantTable;

pub type CodeGenResult = Result<(), Error>;

#[derive(Clone, Debug)]
pub struct Error {
    msg: String,
    token_pos: usize,
    kind: ErrorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    General,
    Unimplemented,
}

#[derive(Debug)]
pub struct CodeGenerator<'a> {
    constant_table: &'a mut ConstantTable,
    bytecode_generator: ByteCodeGen,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(constant_table: &'a mut ConstantTable) -> Self {
        CodeGenerator {
            constant_table,
            bytecode_generator: ByteCodeGen::new(),
        }
    }

    pub fn compile(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        self.visit(node, iseq, use_value)
    }
}

// Visit methods for each Node

impl<'a> CodeGenerator<'a> {
    pub fn visit(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        match node.base {
            NodeBase::StatementList(ref node_list) => {
                self.visit_statement_list(node_list, iseq, use_value)?
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    pub fn visit_statement_list(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        Ok(())
    }
}

// Methods for Error handling

impl Error {
    pub fn new_general_error(msg: String, token_pos: usize) -> Self {
        Error {
            msg,
            token_pos,
            kind: ErrorKind::General,
        }
    }

    pub fn new_unimplemented_error(msg: String, token_pos: usize) -> Self {
        Error {
            msg,
            token_pos,
            kind: ErrorKind::Unimplemented,
        }
    }
}
