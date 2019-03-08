use bytecode_gen::{ByteCode, ByteCodeGenerator, VMInst};
use gc::MemoryAllocator;
use node::{BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, VarKind};
use vm::constant::ConstantTable;
use vm::jsvalue::function::{DestinationKind, Exception};
use vm::jsvalue::value::Value2;
use vm::jsvalue::{prototype, value};

pub type CodeGenResult = Result<(), Error>;

#[derive(Clone, Debug)]
pub struct Error {
    pub msg: String,
    pub token_pos: usize,
    pub kind: ErrorKind,
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
    pub object_prototypes: &'a prototype::ObjectPrototypes,
    pub function_stack: Vec<FunctionInfo>,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: Option<String>,
    pub param_names: Vec<String>,
    pub var_names: Vec<String>,
    pub lex_names: Vec<String>,
    pub func_decls: Vec<Value2>,
    pub level: Vec<Level>,
    pub exception_table: Vec<Exception>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Level {
    Function,
    Block { names: Vec<String> },
    TryOrCatch { finally_jmp_instr_pos: Vec<usize> },
    Finally,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        constant_table: &'a mut ConstantTable,
        memory_allocator: &'a mut MemoryAllocator,
        object_prototypes: &'a prototype::ObjectPrototypes,
    ) -> Self {
        CodeGenerator {
            bytecode_generator: ByteCodeGenerator::new(constant_table),
            function_stack: vec![FunctionInfo::new(None) /* = global */],
            object_prototypes,
            memory_allocator,
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
            NodeBase::Block(ref node_list) => {
                self.visit_block_statement(node_list, iseq, use_value)?
            }
            NodeBase::If(ref cond, ref then, ref else_) => {
                self.visit_if(&*cond, &*then, &*else_, iseq)?
            }
            NodeBase::While(ref cond, ref body) => self.visit_while(&*cond, &*body, iseq)?,
            NodeBase::Try(ref try, ref catch, ref param, ref finally) => {
                self.visit_try(&*try, &*catch, &*param, &*finally, iseq)?
            }
            NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.visit_function_decl(name, params, &*body)?
            }
            NodeBase::FunctionExpr(ref name, ref params, ref body) => {
                self.visit_function_expr(name, params, &*body, iseq, use_value)?
            }
            NodeBase::VarDecl(ref name, ref init, ref kind) => {
                self.visit_var_decl(node, name, init, kind, iseq)?
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit_member(&*parent, property, iseq, use_value)?
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
            NodeBase::Throw(ref val) => self.visit_throw(val, iseq)?,
            NodeBase::Return(ref val) => self.visit_return(val, iseq)?,
            NodeBase::New(ref expr) => self.visit_new(&*expr, iseq, use_value)?,
            NodeBase::Object(ref properties) => self.visit_object_literal(properties, iseq)?,
            NodeBase::Identifier(ref name) => {
                if use_value {
                    self.bytecode_generator.append_get_value(name, iseq)
                }
            }
            NodeBase::String(ref s) => {
                if use_value {
                    self.bytecode_generator
                        .append_push_const(Value2::string(self.memory_allocator, s.clone()), iseq)
                }
            }
            NodeBase::This => {
                if use_value {
                    self.bytecode_generator.append_push_this(iseq);
                }
            }
            NodeBase::Number(n) => self.bytecode_generator.append_push_number(n, iseq),
            NodeBase::Nope => {}
            ref e => unimplemented!("{:?}", e),
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

    fn visit_block_statement(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        let id = self
            .bytecode_generator
            .constant_table
            .add_lex_env_info(vec![]);
        self.bytecode_generator.append_push_env(id as u32, iseq);

        self.current_function().level.push(Level::new_block_level());

        for node in node_list {
            self.visit(node, iseq, use_value)?;
        }

        match self.current_function().level.pop().unwrap() {
            Level::Block { names } => {
                *self
                    .bytecode_generator
                    .constant_table
                    .get_mut(id)
                    .as_lex_env_info_mut() = names;
            }
            _ => unreachable!(),
        };

        self.bytecode_generator.append_pop_env(iseq);

        Ok(())
    }

    fn visit_if(
        &mut self,
        cond: &Node,
        then: &Node,
        else_: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp_if_false(0, iseq);

        self.visit(then, iseq, false)?;

        if else_.base == NodeBase::Nope {
            let pos = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
            );
        } else {
            let then_end_pos = iseq.len() as isize;
            self.bytecode_generator.append_jmp(0, iseq);

            let pos = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
            );

            self.visit(else_, iseq, false)?;

            let pos = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (pos - then_end_pos) as i32 - 5,
                &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
            );
        }

        Ok(())
    }

    pub fn visit_while(&mut self, cond: &Node, body: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        // name:
        //   while(...) {} // <- this while is named 'name'
        // let name = self.state.loop_names.pop();

        let start = iseq.len() as isize;

        // self.bytecode_generator.append_loop_start(iseq);

        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp_if_false(0, iseq);

        self.visit(body, iseq, false)?;

        let loop_pos = iseq.len() as isize;
        self.bytecode_generator
            .append_jmp((start - loop_pos) as i32 - 5, iseq);

        // self.bytecode_generator.replace_int32(
        //     (iseq.len() as i32 - start as i32) - 5,
        //     &mut iseq[start as usize + 1..start as usize + 5],
        // );

        // let break_pos = iseq.len() as isize;
        // self.replace_continue_dsts(&name, start, iseq);
        // self.replace_break_dsts(&name, break_pos, iseq);

        let end = iseq.len() as isize;
        self.bytecode_generator.replace_int32(
            (end - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        Ok(())
    }

    pub fn visit_try(
        &mut self,
        try: &Node,
        catch: &Node,
        param: &Node,
        finally: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        // TODO: Refine code

        let has_catch = catch.base != NodeBase::Nope;

        // Try block
        let (try_, try_to_finally, leave_try) = {
            let try_start = iseq.len() as usize;

            self.current_function()
                .level
                .push(Level::new_try_or_catch_level());

            self.visit(try, iseq, false)?;

            let try_ = self.current_function().level.pop().unwrap();

            let try_to_finally = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let leave_try = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let try_end = iseq.len() as usize;
            self.current_function().exception_table.push(Exception {
                start: try_start,
                end: try_end,
                dst_kind: if has_catch {
                    DestinationKind::Catch
                } else {
                    DestinationKind::Finally
                },
            });

            (try_, try_to_finally, leave_try)
        };

        // Catch block
        let (catch_, catch_to_finally, leave_catch) = if has_catch {
            let catch_start = iseq.len() as usize;
            let param_name = match param.base {
                NodeBase::Identifier(ref name) => name.clone(),
                _ => unimplemented!(),
            };

            self.current_function()
                .level
                .push(Level::new_try_or_catch_level());
            let env_id = self
                .bytecode_generator
                .constant_table
                .add_lex_env_info(vec![]);
            self.bytecode_generator.append_push_env(env_id as u32, iseq);
            self.current_function().level.push(Level::Block {
                names: vec![param_name.clone()],
            });
            self.bytecode_generator.append_set_value(&param_name, iseq);

            self.visit(catch, iseq, false)?;

            self.bytecode_generator.append_pop_env(iseq);

            let names = self.current_function().level.pop().unwrap().as_block();
            let catch_ = self.current_function().level.pop().unwrap();
            *self
                .bytecode_generator
                .constant_table
                .get_mut(env_id)
                .as_lex_env_info_mut() = names;

            let catch_to_finally = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let leave_catch = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let catch_end = iseq.len() as usize;
            self.current_function().exception_table.push(Exception {
                start: catch_start,
                end: catch_end,
                dst_kind: DestinationKind::Finally,
            });

            (catch_, catch_to_finally, leave_catch)
        } else {
            (Level::new_try_or_catch_level(), 0, 0)
        };

        // Finally block
        let finally_start = iseq.len() as usize;

        try_.set_jmp_to_finally(finally_start, &mut self.bytecode_generator, iseq);
        catch_.set_jmp_to_finally(finally_start, &mut self.bytecode_generator, iseq);

        self.current_function().level.push(Level::Finally);
        self.visit(finally, iseq, false)?;
        assert_eq!(self.current_function().level.pop().unwrap(), Level::Finally);
        self.bytecode_generator.append_return_sub(iseq);

        let finally_end = iseq.len() as usize;

        self.bytecode_generator.replace_int32(
            (finally_start - try_to_finally) as i32 - 5,
            &mut iseq[try_to_finally + 1..try_to_finally + 5],
        );
        self.bytecode_generator.replace_int32(
            (finally_end - leave_try) as i32 - 5,
            &mut iseq[leave_try + 1..leave_try + 5],
        );

        if has_catch {
            self.bytecode_generator.replace_int32(
                (finally_start - catch_to_finally) as i32 - 5,
                &mut iseq[catch_to_finally + 1..catch_to_finally + 5],
            );
            self.bytecode_generator.replace_int32(
                (finally_end - leave_catch) as i32 - 5,
                &mut iseq[leave_catch + 1..leave_catch + 5],
            );
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
        self.current_function().var_names.push(name.clone());
        self.current_function().func_decls.push(func);
        Ok(())
    }

    fn visit_function_expr(
        &mut self,
        name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        let func = self.visit_function(name.clone(), params, body)?;
        self.bytecode_generator.append_push_const(func, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

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
            self.object_prototypes,
            function_info.name,
            params,
            function_info.var_names,
            function_info.lex_names,
            function_info.func_decls,
            func_iseq,
            function_info.exception_table,
        ))
    }

    pub fn visit_var_decl(
        &mut self,
        node: &Node,
        name: &String,
        init: &Option<Box<Node>>,
        kind: &VarKind,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        fn let_decl(codegen: &mut CodeGenerator, node: &Node, name: String) -> CodeGenResult {
            let cur_func = codegen.function_stack.last_mut().unwrap();
            let cur_level = cur_func.level.last_mut().unwrap();
            let names = match cur_level {
                Level::Function => &mut cur_func.lex_names,
                Level::Block { ref mut names } => names,
                _ => unreachable!(),
            };
            if names.iter().find(|declared| *declared == &name).is_some() {
                return Err(Error::new_general_error(
                    format!("Identifier '{}' has already been declared", name),
                    node.pos,
                ));
            }
            names.push(name);
            Ok(())
        }

        // let mut is_initialized = false;

        if let &Some(ref init) = init {
            self.visit(&*init, iseq, true)?;
            self.bytecode_generator.append_set_value(name, iseq);
            // is_initialized = true;
        }

        match kind {
            VarKind::Var => {
                self.function_stack
                    .last_mut()
                    .unwrap()
                    .var_names
                    .push(name.clone());
            }
            VarKind::Let => let_decl(self, node, name.clone())?,
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn visit_member(
        &mut self,
        parent: &Node,
        member: &String,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        self.visit(parent, iseq, true)?;
        let property = Value2::string(self.memory_allocator, member.clone());
        self.bytecode_generator.append_push_const(property, iseq);
        self.bytecode_generator.append_get_member(iseq);

        Ok(())
    }

    pub fn visit_binary_op(
        &mut self,
        lhs: &Node,
        rhs: &Node,
        op: &BinOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
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

        match callee.base {
            NodeBase::Member(ref parent, ref property_name) => {
                self.bytecode_generator.append_push_const(
                    Value2::string(self.memory_allocator, property_name.clone()),
                    iseq,
                );
                self.visit(&*parent, iseq, true)?;
                self.bytecode_generator
                    .append_call_method(args.len() as u32, iseq);
            }
            _ => {
                self.visit(callee, iseq, true)?;
                self.bytecode_generator.append_call(args.len() as u32, iseq);
            }
        }

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_throw(&mut self, val: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        self.visit(val, iseq, true)?;

        if self.current_function().in_try_or_catch() {
            self.unwind_try_or_catch(iseq);
        } else if self.current_function().in_finally() {
            self.unwind_finally(iseq);
        }

        self.bytecode_generator.append_throw(iseq);
        Ok(())
    }

    fn visit_return(&mut self, val: &Option<Box<Node>>, iseq: &mut ByteCode) -> CodeGenResult {
        if let Some(val) = val {
            self.visit(val, iseq, true)?
        } else {
            self.bytecode_generator.append_push_undefined(iseq);
        }

        if self.current_function().in_try_or_catch() {
            self.current_function()
                .get_last_try_or_catch()
                .as_try_or_catch_mut()
                .push(iseq.len() as usize);
            self.bytecode_generator.append_return_try(iseq);
        } else {
            self.bytecode_generator.append_return(iseq);
        }

        Ok(())
    }

    fn visit_new(&mut self, expr: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        let (callee, args) = match expr.base {
            NodeBase::Call(ref callee, ref args) => (&*callee, args),
            _ => unimplemented!(),
        };

        for arg in args.iter().rev() {
            self.visit(arg, iseq, true)?
        }

        match callee.base {
            NodeBase::Member(ref parent, ref property_name) => {
                self.visit(parent, iseq, true)?;
                let property = Value2::string(self.memory_allocator, property_name.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.bytecode_generator.append_get_member(iseq);
            }
            _ => {
                self.visit(callee, iseq, true)?;
            }
        }

        self.bytecode_generator.append_construct(args.len(), iseq);

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        properties: &Vec<PropertyDefinition>,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        for property in properties {
            match property {
                PropertyDefinition::IdentifierReference(name) => {
                    self.bytecode_generator.append_get_value(name, iseq);
                    self.bytecode_generator.append_push_const(
                        Value2::string(self.memory_allocator, name.clone()),
                        iseq,
                    );
                }
                PropertyDefinition::Property(name, node) => {
                    self.visit(&node, iseq, true)?;
                    self.bytecode_generator.append_push_const(
                        Value2::string(self.memory_allocator, name.clone()),
                        iseq,
                    );
                }
            }
        }

        self.bytecode_generator
            .append_create_object(properties.len() as usize, iseq);

        Ok(())
    }
}

impl<'a> CodeGenerator<'a> {
    fn assign_stack_top_to(&mut self, dst: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.bytecode_generator.append_set_value(name, iseq);
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit(&*parent, iseq, true)?;
                let property = Value2::string(self.memory_allocator, property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.bytecode_generator.append_set_member(iseq);
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn current_function(&mut self) -> &mut FunctionInfo {
        self.function_stack.last_mut().unwrap()
    }

    fn unwind_try_or_catch(&mut self, iseq: &mut ByteCode) {
        for level in self.current_function().level.clone().iter().rev() {
            match level {
                &Level::TryOrCatch { .. } => break,
                &Level::Block { .. } => self.bytecode_generator.append_pop_env(iseq),
                _ => {}
            }
        }
    }

    fn unwind_finally(&mut self, iseq: &mut ByteCode) {
        for level in self.current_function().level.clone().iter().rev() {
            match level {
                &Level::Finally => break,
                &Level::Block { .. } => self.bytecode_generator.append_pop_env(iseq),
                _ => {}
            }
        }
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
            level: vec![Level::Function],
            exception_table: vec![],
        }
    }

    pub fn in_try_or_catch(&self) -> bool {
        self.level
            .iter()
            .rev()
            .find(|level| match level {
                &Level::TryOrCatch { .. } => true,
                _ => false,
            })
            .is_some()
    }

    pub fn in_finally(&self) -> bool {
        self.level
            .iter()
            .rev()
            .find(|level| *level == &Level::Finally)
            .is_some()
    }

    pub fn get_last_try_or_catch(&mut self) -> &mut Level {
        self.level
            .iter_mut()
            .rev()
            .find(|level| match level {
                &Level::TryOrCatch { .. } => true,
                _ => false,
            })
            .unwrap()
    }
}

// Level

impl Level {
    pub fn new_function_level() -> Self {
        Level::Function
    }

    pub fn new_block_level() -> Self {
        Level::Block { names: vec![] }
    }

    pub fn new_try_or_catch_level() -> Self {
        Level::TryOrCatch {
            finally_jmp_instr_pos: vec![],
        }
    }

    pub fn as_block(self) -> Vec<String> {
        match self {
            Level::Block { names } => names,
            _ => panic!(),
        }
    }

    pub fn as_try_or_catch(self) -> Vec<usize> {
        match self {
            Level::TryOrCatch {
                finally_jmp_instr_pos,
            } => finally_jmp_instr_pos,
            _ => panic!(),
        }
    }

    pub fn as_try_or_catch_mut(&mut self) -> &mut Vec<usize> {
        match self {
            Level::TryOrCatch {
                ref mut finally_jmp_instr_pos,
            } => finally_jmp_instr_pos,
            _ => panic!(),
        }
    }

    pub fn set_jmp_to_finally(
        self,
        dst: usize,
        bytecode_generator: &mut ByteCodeGenerator,
        iseq: &mut ByteCode,
    ) {
        let finally_jmp_instr_pos = self.as_try_or_catch();
        for instr_pos in finally_jmp_instr_pos {
            assert!(match iseq[instr_pos] {
                VMInst::RETURN | VMInst::JMP => true,
                _ => false,
            });
            bytecode_generator.replace_int32(
                (dst - instr_pos) as i32 - 5,
                &mut iseq[instr_pos as usize + 1..instr_pos as usize + 5],
            );
        }
    }
}
