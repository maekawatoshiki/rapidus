use bytecode_gen::{ByteCode, ByteCodeGenerator};
use gc::MemoryAllocator;
use node::{Node, NodeBase};
use vm::constant::ConstantTable;
use vm::frame;

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
    pub bytecode_generator: ByteCodeGenerator<'a>,
    pub memory_allocator: &'a mut MemoryAllocator,
    pub function_stack: Vec<FunctionInfo>,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: Option<String>,
    pub param_names: Vec<String>,
    pub var_names: Vec<String>,
    pub lex_names: Vec<String>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        constant_table: &'a mut ConstantTable,
        memory_allocator: &'a mut MemoryAllocator,
    ) -> Self {
        CodeGenerator {
            bytecode_generator: ByteCodeGenerator::new(constant_table),
            memory_allocator,
            function_stack: vec![FunctionInfo::new()],
        }
    }

    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<FunctionInfo, Error> {
        self.visit(node, iseq, use_value)?;
        self.bytecode_generator.append_end(iseq);
        Ok(self.function_stack[0].clone() /* = global */)
    }
}

// Visit methods for each Node

impl<'a> CodeGenerator<'a> {
    fn visit(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        match node.base {
            NodeBase::StatementList(ref node_list) => {
                self.visit_statement_list(node_list, iseq, use_value)?
            }
            NodeBase::Assign(ref dst, ref src) => {
                self.visit_assign(&*dst, &*src, iseq, use_value)?
            }
            NodeBase::Number(n) => self.bytecode_generator.append_push_number(n, iseq),
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn visit_statement_list(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        for node in node_list {
            self.visit(node, iseq, use_value)?;
        }

        Ok(())
    }

    fn visit_assign(
        &mut self,
        dst: &Node,
        src: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        self.visit(src, iseq, true)?;

        if use_value {
            self.bytecode_generator.append_double(iseq);
        }

        self.assign_stack_top_to(dst, iseq)?;

        Ok(())
    }
}

impl<'a> CodeGenerator<'a> {
    fn assign_stack_top_to(&mut self, dst: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.bytecode_generator.append_set_value(name, iseq);
            }
            _ => unimplemented!(),
        }

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

// FunctionInfo

impl FunctionInfo {
    pub fn new() -> Self {
        FunctionInfo {
            name: None,
            var_names: vec![],
            lex_names: vec![],
            param_names: vec![],
        }
    }
}
