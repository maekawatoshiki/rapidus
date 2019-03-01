use bytecode_gen::{ByteCode, ByteCodeGenerator};
use gc::MemoryAllocator;
use node::{
    BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp, VarKind,
};
use vm::constant::ConstantTable;
use vm::jsvalue::value;
use vm::jsvalue::value::Value2;

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
    pub func_decls: Vec<Value2>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        constant_table: &'a mut ConstantTable,
        memory_allocator: &'a mut MemoryAllocator,
    ) -> Self {
        CodeGenerator {
            bytecode_generator: ByteCodeGenerator::new(constant_table),
            memory_allocator,
            function_stack: vec![FunctionInfo::new(None) /* = global */],
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
            NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.visit_function_decl(name, params, &*body)?
            }
            NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.visit_binary_op(&*lhs, &*rhs, op, iseq, use_value)?
            }
            NodeBase::Assign(ref dst, ref src) => {
                self.visit_assign(&*dst, &*src, iseq, use_value)?
            }
            NodeBase::Call(ref callee, ref args) => {
                self.visit_call(&*callee, args, iseq, use_value)?
            }
            NodeBase::Identifier(ref name) => self.bytecode_generator.append_get_value(name, iseq),
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

    fn visit_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
    ) -> CodeGenResult {
        let func = self.visit_function(Some(name.clone()), params, body)?;
        self.function_stack
            .last_mut()
            .unwrap()
            .func_decls
            .push(func);
        Ok(())
    }

    fn visit_function(
        &mut self,
        name: Option<String>,
        params: &FormalParameters,
        body: &Node,
    ) -> Result<Value2, Error> {
        self.function_stack.push(FunctionInfo::new(name));

        let mut func_iseq = vec![];

        self.visit(body, &mut func_iseq, false)?;

        self.bytecode_generator
            .append_push_undefined(&mut func_iseq);
        self.bytecode_generator.append_return(&mut func_iseq);

        let params = params
            .clone()
            .iter()
            .map(
                |FormalParameter {
                     name,
                     is_rest_param,
                     ..
                 }| value::FunctionParameter {
                    name: name.clone(),
                    is_rest_param: *is_rest_param,
                },
            )
            .collect();

        let function_info = self.function_stack.pop().unwrap();

        Ok(Value2::function(
            self.memory_allocator,
            function_info.name,
            params,
            function_info.var_names,
            function_info.lex_names,
            function_info.func_decls,
            func_iseq,
        ))
    }

    pub fn visit_binary_op(
        &mut self,
        lhs: &Node,
        rhs: &Node,
        op: &BinOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        if !use_value {
            return Ok(());
        }

        self.visit(lhs, iseq, true)?;
        self.visit(rhs, iseq, true)?;

        match op {
            &BinOp::Add => self.bytecode_generator.append_add(iseq),
            &BinOp::Sub => self.bytecode_generator.append_sub(iseq),
            &BinOp::Mul => self.bytecode_generator.append_mul(iseq),
            &BinOp::Div => self.bytecode_generator.append_div(iseq),
            &BinOp::Rem => self.bytecode_generator.append_rem(iseq),
            &BinOp::Eq => self.bytecode_generator.append_eq(iseq),
            &BinOp::Ne => self.bytecode_generator.append_ne(iseq),
            &BinOp::SEq => self.bytecode_generator.append_seq(iseq),
            &BinOp::SNe => self.bytecode_generator.append_sne(iseq),
            &BinOp::And => self.bytecode_generator.append_and(iseq),
            &BinOp::Or => self.bytecode_generator.append_or(iseq),
            &BinOp::Xor => self.bytecode_generator.append_xor(iseq),
            &BinOp::Lt => self.bytecode_generator.append_lt(iseq),
            &BinOp::Gt => self.bytecode_generator.append_gt(iseq),
            &BinOp::Le => self.bytecode_generator.append_le(iseq),
            &BinOp::Ge => self.bytecode_generator.append_ge(iseq),
            &BinOp::Shl => self.bytecode_generator.append_shl(iseq),
            &BinOp::Shr => self.bytecode_generator.append_shr(iseq),
            &BinOp::ZFShr => self.bytecode_generator.append_zfshr(iseq),
            _ => {}
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

    fn visit_call(
        &mut self,
        callee: &Node,
        args: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        for arg in args.iter().rev() {
            self.visit(arg, iseq, true)?
        }

        self.visit(callee, iseq, true)?;

        self.bytecode_generator.append_call(args.len() as u32, iseq);

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

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
    pub fn new(name: Option<String>) -> Self {
        FunctionInfo {
            name,
            var_names: vec![],
            lex_names: vec![],
            func_decls: vec![],
            param_names: vec![],
        }
    }
}
