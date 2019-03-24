use bytecode_gen::{ByteCode, ByteCodeGen, VMInst};
use node::{
    BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp, VarKind,
};
use std::collections::VecDeque;
use vm::callobj::CallObject;
use vm::value::*;

#[derive(Clone, Debug)]
pub enum Error {
    General { msg: String, token_pos: usize },
    Unimplemented { msg: String, token_pos: usize },
}

#[derive(Clone, Debug)]
pub enum Level {
    Function,
    Try {
        /// destination position in iseq for RETURN_TRY.
        return_instr_pos: Vec<isize>,
    },
    Catch {
        /// destination position in iseq for RETURN_TRY.
        return_instr_pos: Vec<isize>,
    },
    Finally,
}

impl Level {
    pub fn set_finally(self, finally_pos: isize, gen: &mut ByteCodeGen, iseq: &mut ByteCode) {
        match self {
            Level::Try { return_instr_pos } | Level::Catch { return_instr_pos } => {
                for instr_pos in return_instr_pos {
                    gen.replace_int32(
                        (finally_pos - instr_pos) as i32,
                        &mut iseq[instr_pos as usize + 1..instr_pos as usize + 5],
                    );
                }
            }
            _ => {}
        }
    }
}

#[derive(Clone, Debug)]
pub struct CodegenState {
    func_header_info: Vec<VecDeque<(String, DeclSection)>>,
    block_header_info: Vec<VecDeque<(String, DeclSection)>>,
    continue_inst_positions: Vec<(Option<String>, isize, usize, usize, usize)>, //(label_name, inst_pos, scope_level, try_level, token_pos)
    break_inst_positions: Vec<(Option<String>, isize, usize, usize, usize)>,
    loop_names: Vec<String>,
    try_level: Vec<Level>,
    scope_level: usize,
}

impl CodegenState {
    pub fn new() -> Self {
        CodegenState {
            func_header_info: vec![VecDeque::default()],
            block_header_info: vec![VecDeque::default()],
            continue_inst_positions: vec![],
            break_inst_positions: vec![],
            loop_names: vec![],
            try_level: vec![],
            scope_level: 1,
        }
    }

    pub fn check_unresolved_label(&self) -> Result<(), Error> {
        if self.break_inst_positions.len() != 0 {
            let token_pos = self.break_inst_positions[0].3;
            return Err(Error::General {
                msg: "undefined destination label.".to_string(),
                token_pos,
            });
        };
        if self.continue_inst_positions.len() != 0 {
            let token_pos = self.continue_inst_positions[0].3;
            return Err(Error::General {
                msg: "undefined destination label.".to_string(),
                token_pos,
            });
        };
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeclSection {
    DeclFunc(Value),
    DeclVar,
    DeclConst,
    DeclLet,
}

#[derive(Clone, Debug)]
pub struct VMCodeGen {
    pub global_varmap: CallObjectRef,
    pub bytecode_gen: ByteCodeGen,
    pub state: CodegenState,
    pub state_stack: Vec<CodegenState>,
}

impl VMCodeGen {
    pub fn new(global: CallObjectRef) -> VMCodeGen {
        VMCodeGen {
            global_varmap: global,
            bytecode_gen: ByteCodeGen::new(),
            state: CodegenState::new(),
            state_stack: vec![],
        }
    }
}

impl VMCodeGen {
    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        self.bytecode_gen.gen_create_context(iseq);

        self.run(node, iseq, use_value)?;

        self.bytecode_gen.gen_end(iseq);

        self.set_function_header(iseq);
        self.state.check_unresolved_label()?;

        Ok(())
    }

    fn run(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> Result<(), Error> {
        /*
                if let Some(constant) = node.base.fold_num_consts() {
                    match constant {
                        NodeBase::String(ref s) => self
                            .bytecode_gen
                            .gen_push_const(Value::string(s.clone()), iseq),
                        NodeBase::Number(n) => self.bytecode_gen.gen_push_number(n, iseq),
                        NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, iseq),
                        // TODO
                        _ => unreachable!(),
                    }
                    return Ok(());
                }
        */
        match &node.base {
            &NodeBase::StatementList(ref node_list) => {
                self.run_statement_list(node_list, iseq, use_value)?
            }
            &NodeBase::Block(ref node_list) => {
                self.run_block_statement(node_list, iseq, use_value)?
            }
            &NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.run_function_decl(name, params, &*body)?
            }
            &NodeBase::FunctionExpr(ref name, ref params, ref body) => {
                self.run_function_expr(name, params, &*body, iseq)?
            }
            &NodeBase::VarDecl(ref name, ref init, ref var_kind) => {
                self.run_var_decl(name, init, iseq, var_kind, &node)?
            }
            &NodeBase::If(ref cond, ref then_, ref else_) => {
                self.run_if(&*cond, &*then_, &*else_, iseq)?
            }
            &NodeBase::While(ref cond, ref body) => self.run_while(&*cond, &*body, iseq)?,
            &NodeBase::For(ref init, ref cond, ref step, ref body) => {
                self.run_for(&*init, &*cond, &*step, &*body, iseq)?
            }
            &NodeBase::Assign(ref dst, ref src) => {
                self.run_assign(&*dst, &*src, iseq, use_value)?
            }
            &NodeBase::UnaryOp(ref expr, ref op) => {
                self.run_unary_op(&*expr, op, iseq, use_value)?
            }
            &NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.run_binary_op(&*lhs, &*rhs, op, iseq)?
            }
            &NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                self.run_ternary_op(&*cond, &*then, &*else_, iseq)?
            }
            &NodeBase::Label(ref name, ref body) => self.run_label(name, &*body, iseq)?,
            &NodeBase::Call(ref callee, ref args) => {
                self.run_call(&*callee, args, iseq, use_value)?
            }
            &NodeBase::Member(ref parent, ref member) => self.run_member(&*parent, member, iseq)?,
            &NodeBase::Index(ref parent, ref idx) => self.run_index(&*parent, &*idx, iseq)?,
            &NodeBase::Return(ref val) => self.run_return(val, iseq)?,
            &NodeBase::Break(ref name) => self.run_break(name, iseq, node.pos)?,
            &NodeBase::Continue(ref name) => self.run_continue(name, iseq, node.pos)?,
            &NodeBase::Throw(ref val) => self.run_throw(val, iseq)?,
            &NodeBase::Try(ref try, ref catch, ref param, ref finally) => {
                self.run_try(&*try, &*catch, &*param, &*finally, iseq)?
            }
            &NodeBase::New(ref expr) => self.run_new_expr(&*expr, iseq)?,
            &NodeBase::Object(ref properties) => self.run_object_literal(properties, iseq)?,
            &NodeBase::Array(ref properties) => self.run_array_literal(properties, iseq)?,
            &NodeBase::Identifier(ref name) => self.run_identifier(name, iseq)?,
            &NodeBase::This => self.bytecode_gen.gen_push_this(iseq),
            &NodeBase::Arguments => self.bytecode_gen.gen_push_arguments(iseq),
            &NodeBase::Undefined => self.bytecode_gen.gen_push_undefined(iseq),
            &NodeBase::Null => self.bytecode_gen.gen_push_const(Value::Null, iseq),
            &NodeBase::String(ref s) => self
                .bytecode_gen
                .gen_push_const(Value::string(s.clone()), iseq),
            &NodeBase::Number(n) => self.bytecode_gen.gen_push_number(n, iseq),
            &NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, iseq),
            &NodeBase::Nope if use_value => {
                self.bytecode_gen.gen_push_const(Value::empty(), iseq);
            }
            &NodeBase::Nope => {}
        }

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_statement_list(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        for node in node_list {
            self.run(node, iseq, use_value)?;
        }

        Ok(())
    }

    pub fn run_block_statement(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        self.state.scope_level += 1;
        self.bytecode_gen.gen_push_scope(iseq);
        let jmp_pos = iseq.len();
        self.bytecode_gen.gen_jmp(0, iseq);
        let start_pos = iseq.len();

        self.state.block_header_info.push(VecDeque::default());
        for node in node_list {
            self.run(node, iseq, use_value)?;
        }

        self.state.scope_level -= 1;
        self.bytecode_gen.gen_pop_scope(iseq);
        let jmp2_pos = iseq.len();
        self.bytecode_gen.gen_jmp(0, iseq);

        let header_pos = iseq.len();
        self.bytecode_gen.replace_int32(
            (header_pos - jmp_pos) as i32 - 5,
            &mut iseq[jmp_pos as usize + 1..jmp_pos as usize + 5],
        );

        let mut block_header_info = self.state.block_header_info.pop().unwrap();
        while let Some(header) = block_header_info.pop_front() {
            match header {
                (func_name, DeclSection::DeclFunc(val)) => {
                    self.bytecode_gen.gen_decl_var(&func_name, iseq);
                    self.bytecode_gen.gen_push_const(val, iseq);
                    self.bytecode_gen.gen_update_parent_scope(iseq);
                    self.bytecode_gen.gen_set_value(&func_name, iseq);
                }
                (_var_name, DeclSection::DeclVar) => {
                    //self.bytecode_gen.gen_decl_var(&var_name, iseq);
                }
                (var_name, DeclSection::DeclConst) => {
                    self.bytecode_gen.gen_decl_const(&var_name, iseq);
                }
                (var_name, DeclSection::DeclLet) => {
                    self.bytecode_gen.gen_decl_let(&var_name, iseq);
                }
            }
        }

        let jmp3_pos = iseq.len();
        self.bytecode_gen
            .gen_jmp(start_pos as i32 - jmp3_pos as i32 - 5, iseq);

        let exit_pos = iseq.len();
        self.bytecode_gen.replace_int32(
            (exit_pos - jmp2_pos) as i32 - 5,
            &mut iseq[jmp2_pos as usize + 1..jmp2_pos as usize + 5],
        );

        Ok(())
    }
}

impl VMCodeGen {
    fn set_function_header(&mut self, iseq: &mut ByteCode) {
        let mut header_iseq = vec![];
        let mut header_info = VecDeque::default();
        header_info.append(self.state.func_header_info.last_mut().unwrap());
        header_info.append(self.state.block_header_info.last_mut().unwrap());
        while let Some(header) = header_info.pop_front() {
            match header {
                (func_name, DeclSection::DeclFunc(val)) => {
                    self.bytecode_gen.gen_decl_var(&func_name, &mut header_iseq);
                    self.bytecode_gen.gen_push_const(val, &mut header_iseq);
                    self.bytecode_gen.gen_update_parent_scope(&mut header_iseq);
                    self.bytecode_gen
                        .gen_set_value(&func_name, &mut header_iseq);
                }
                (var_name, DeclSection::DeclVar) => {
                    self.bytecode_gen.gen_decl_var(&var_name, &mut header_iseq);
                }
                (var_name, DeclSection::DeclConst) => {
                    self.bytecode_gen
                        .gen_decl_const(&var_name, &mut header_iseq);
                }
                (var_name, DeclSection::DeclLet) => {
                    self.bytecode_gen.gen_decl_let(&var_name, &mut header_iseq);
                }
            }
        }
        iseq.splice(1..1, header_iseq);
    }

    /// function name(params...) { body }
    pub fn run_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
    ) -> Result<(), Error> {
        let val = self.run_function(params, body)?;

        self.state
            .func_header_info
            .last_mut()
            .unwrap()
            .push_back((name.clone(), DeclSection::DeclFunc(val)));

        Ok(())
    }

    /// function(params) { body }
    pub fn run_function_expr(
        &mut self,
        // TODO: _name should be used.
        _name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        let val = self.run_function(params, body)?;

        self.bytecode_gen.gen_push_const(val, iseq);
        self.bytecode_gen.gen_update_parent_scope(iseq);

        Ok(())
    }

    /// parse the function node and convert to the function object.
    fn run_function(&mut self, params: &FormalParameters, body: &Node) -> Result<Value, Error> {
        self.state_stack.push(self.state.clone());
        self.state = CodegenState::new();

        let new_callobj = CallObject::new_with_this(Value::object(self.global_varmap.vals.clone()));

        let mut func_iseq = vec![];

        self.bytecode_gen.gen_create_context(&mut func_iseq);

        self.run(body, &mut func_iseq, false)?;

        self.bytecode_gen.gen_push_undefined(&mut func_iseq);
        self.bytecode_gen.gen_return(&mut func_iseq);

        let params = params
            .clone()
            .iter()
            .map(
                |FormalParameter {
                     name,
                     is_rest_param,
                     ..
                 }| (name.clone(), *is_rest_param),
            )
            .collect();

        self.set_function_header(&mut func_iseq);
        self.state.check_unresolved_label()?;

        let val = Value::function(func_iseq.clone(), params, new_callobj);

        self.state = self.state_stack.pop().unwrap();

        Ok(val)
    }

    pub fn run_return(
        &mut self,
        val: &Option<Box<Node>>,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        if let &Some(ref val) = val {
            self.run(&*val, iseq, true)?
        } else {
            self.bytecode_gen.gen_push_undefined(iseq);
        }

        if self.state.try_level.len() == 0 {
            self.bytecode_gen.gen_return(iseq);
        } else {
            match self.state.try_level.last_mut().unwrap() {
                // when in try or catch clause, generate RETERN_TRY.
                Level::Catch {
                    ref mut return_instr_pos,
                }
                | Level::Try {
                    ref mut return_instr_pos,
                } => {
                    return_instr_pos.push(iseq.len() as isize);
                    self.bytecode_gen.gen_return_try(iseq);
                }
                // otherwise, generate RETURN.
                _ => {
                    self.bytecode_gen.gen_return(iseq);
                }
            }
        }

        Ok(())
    }

    pub fn run_throw(&mut self, val: &Node, iseq: &mut ByteCode) -> Result<(), Error> {
        self.run(val, iseq, true)?;
        self.bytecode_gen.gen_throw(iseq);

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_break(
        &mut self,
        name: &Option<String>,
        iseq: &mut ByteCode,
        token_pos: usize,
    ) -> Result<(), Error> {
        let break_inst_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_unwind(0, 0, 0, iseq);

        self.state.break_inst_positions.push((
            name.clone(),
            break_inst_pos,
            self.state.scope_level,
            self.state.try_level.len(),
            token_pos,
        ));

        Ok(())
    }

    pub fn run_continue(
        &mut self,
        name: &Option<String>,
        iseq: &mut ByteCode,
        token_pos: usize,
    ) -> Result<(), Error> {
        let continue_inst_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_unwind(0, 0, 0, iseq);

        self.state.continue_inst_positions.push((
            name.clone(),
            continue_inst_pos,
            self.state.scope_level,
            self.state.try_level.len(),
            token_pos,
        ));

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_new_expr(&mut self, expr: &Node, iseq: &mut ByteCode) -> Result<(), Error> {
        // TODO: Make sure ``expr`` is Call or Identifier
        self.run(expr, iseq, true)?;

        let len = iseq.len();
        if iseq[len - 1 - 4] == VMInst::CALL {
            iseq[len - 1 - 4] = VMInst::CONSTRUCT;
        } else {
            // TODO
            unreachable!()
        }

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_var_decl(
        &mut self,
        name: &String,
        init: &Option<Box<Node>>,
        iseq: &mut ByteCode,
        var_kind: &VarKind,
        node: &Node,
    ) -> Result<(), Error> {
        fn check_duplicate_decl_in_block(
            self_: &mut VMCodeGen,
            name: &String,
            pos: usize,
            kind: DeclSection,
        ) -> Result<(), Error> {
            let info = self_.state.block_header_info.last_mut().unwrap();
            match info.iter().find(|x| &x.0 == name) {
                None => {}
                Some(_) => {
                    return Err(Error::General {
                        msg: format!("Identifier '{}' has already been declared", name),
                        token_pos: pos,
                    });
                }
            }
            info.push_back((name.clone(), kind));
            Ok(())
        }

        let mut is_initialized = false;
        if let &Some(ref init) = init {
            self.run(&*init, iseq, true)?;
            self.bytecode_gen.gen_set_value(name, iseq);
            is_initialized = true;
        };

        match var_kind {
            VarKind::Var => {
                // check duplicate variables in all block variables.
                for info in &self.state.block_header_info {
                    match info
                        .iter()
                        .find(|x| x.1 != DeclSection::DeclVar && &x.0 == name)
                    {
                        None => {}
                        Some(_) => {
                            return Err(Error::General {
                                msg: format!("Identifier '{}' has already been declared", name),
                                token_pos: node.pos,
                            });
                        }
                    }
                }
                self.state
                    .func_header_info
                    .last_mut()
                    .unwrap()
                    .push_back((name.clone(), DeclSection::DeclVar));
                self.state
                    .block_header_info
                    .last_mut()
                    .unwrap()
                    .push_back((name.clone(), DeclSection::DeclVar));
            }
            VarKind::Let => {
                // check duplicate variables in the current block.
                check_duplicate_decl_in_block(self, name, node.pos, DeclSection::DeclLet)?;
            }
            VarKind::Const => {
                if !is_initialized {
                    return Err(Error::General {
                        msg: format!("missing initializer in const declaration"),
                        token_pos: node.pos,
                    });
                }
                check_duplicate_decl_in_block(self, name, node.pos, DeclSection::DeclConst)?;
            }
        }

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_if(
        &mut self,
        cond: &Node,
        then_: &Node,
        else_: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.run(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(then_, iseq, false)?;

        if else_.base == NodeBase::Nope {
            let pos = iseq.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
            );
        } else {
            let then_end_pos = iseq.len() as isize;
            self.bytecode_gen.gen_jmp(0, iseq);

            let pos = iseq.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
            );

            self.run(else_, iseq, false)?;

            let pos = iseq.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - then_end_pos) as i32 - 5,
                &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
            );
        }

        Ok(())
    }

    pub fn run_while(
        &mut self,
        cond: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        // name:
        //   while(...) {} // <- this while is named 'name'
        let name = self.state.loop_names.pop();

        let pos1 = iseq.len() as isize;

        self.bytecode_gen.gen_loop_start(iseq);

        self.run(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq, false)?;

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen
            .gen_jmp((pos1 - loop_pos) as i32 - 5, iseq);

        self.bytecode_gen.replace_int32(
            iseq.len() as i32 - pos1 as i32,
            &mut iseq[pos1 as usize + 1..pos1 as usize + 5],
        );

        let break_pos = iseq.len() as isize;
        self.replace_continue_dsts(&name, pos1, iseq);
        self.replace_break_dsts(&name, break_pos, iseq);

        let pos2 = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos2 - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        Ok(())
    }

    pub fn run_for(
        &mut self,
        init: &Node,
        cond: &Node,
        step: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        // name:
        //   for(...) {} // <- this for is named 'name'
        let name = self.state.loop_names.pop();

        self.run(init, iseq, false)?;

        let pos = iseq.len() as isize;

        self.bytecode_gen.gen_loop_start(iseq);

        self.run(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq, false)?;

        let continue_pos = iseq.len() as isize;
        self.replace_continue_dsts(&name, continue_pos, iseq);

        self.run(step, iseq, false)?;

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp((pos - loop_pos) as i32 - 5, iseq);

        self.bytecode_gen.replace_int32(
            iseq.len() as i32 - pos as i32,
            &mut iseq[pos as usize + 1..pos as usize + 5],
        );

        let break_pos = iseq.len() as isize;

        self.replace_break_dsts(&name, break_pos, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        Ok(())
    }

    pub fn run_label(
        &mut self,
        name: &String,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.state.loop_names.push(name.clone());

        self.run(body, iseq, false)?;

        Ok(())
    }
}
impl VMCodeGen {
    pub fn run_try(
        &mut self,
        try: &Node,
        catch: &Node,
        param: &Node,
        finally: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        let enter_pos = iseq.len() as isize;
        self.bytecode_gen
            .gen_enter_try(iseq, self.state.scope_level);

        self.state.try_level.push(Level::Try {
            return_instr_pos: vec![],
        });
        self.run(try, iseq, false)?;

        let try_ = self.state.try_level.pop().unwrap();

        self.bytecode_gen.gen_jmp(0, iseq);

        let catch_pos = iseq.len() as isize;
        self.bytecode_gen.gen_catch(iseq);

        self.state.scope_level += 1;
        self.bytecode_gen.gen_push_scope(iseq);

        match &param.base {
            NodeBase::Identifier(param_name) => {
                self.bytecode_gen.gen_decl_var(&param_name, iseq);
                self.bytecode_gen.gen_set_value(&param_name, iseq);
            }
            _ => {
                self.bytecode_gen.gen_pop(iseq);
            }
        }
        self.state.try_level.push(Level::Catch {
            return_instr_pos: vec![],
        });
        self.run(catch, iseq, false)?;

        self.state.scope_level -= 1;
        self.bytecode_gen.gen_pop_scope(iseq);

        let catch_ = self.state.try_level.pop().unwrap();

        let finally_pos = iseq.len() as isize;
        self.bytecode_gen.gen_finally(iseq);

        self.state.try_level.push(Level::Finally);
        self.run(finally, iseq, false)?;
        self.state.try_level.pop();

        self.bytecode_gen.gen_leave_try(iseq);

        self.bytecode_gen.replace_int32(
            (catch_pos - enter_pos) as i32,
            &mut iseq[enter_pos as usize + 1..enter_pos as usize + 5],
        );
        self.bytecode_gen.replace_int32(
            (finally_pos - enter_pos) as i32,
            &mut iseq[enter_pos as usize + 5..enter_pos as usize + 9],
        );

        self.bytecode_gen.replace_int32(
            (finally_pos - catch_pos) as i32,
            &mut iseq[catch_pos as usize - 4..catch_pos as usize],
        );

        try_.set_finally(finally_pos, &mut self.bytecode_gen, iseq);
        catch_.set_finally(finally_pos, &mut self.bytecode_gen, iseq);

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_unary_op(
        &mut self,
        expr: &Node,
        op: &UnaryOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        self.run(expr, iseq, true)?;
        match op {
            &UnaryOp::Plus => self.bytecode_gen.gen_posi(iseq),
            &UnaryOp::Minus => self.bytecode_gen.gen_neg(iseq),
            &UnaryOp::Not => self.bytecode_gen.gen_lnot(iseq),
            &UnaryOp::BitwiseNot => self.bytecode_gen.gen_not(iseq),
            &UnaryOp::PrInc => {
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_add(iseq);
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.assign_stack_top(expr, iseq)?
            }
            &UnaryOp::PoInc => {
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_add(iseq);
                self.assign_stack_top(expr, iseq)?
            }
            &UnaryOp::PrDec => {
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_sub(iseq);
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.assign_stack_top(expr, iseq)?
            }
            &UnaryOp::PoDec => {
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_sub(iseq);
                self.assign_stack_top(expr, iseq)?
            }
            op => {
                return Err(Error::Unimplemented {
                    msg: format!("error: unary operator '{:?}' is unimplemented", op),
                    token_pos: expr.pos,
                });
            }
        }

        Ok(())
    }

    pub fn run_binary_op(
        &mut self,
        lhs: &Node,
        rhs: &Node,
        op: &BinOp,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        // Following code has influence on JIT(src/jit.rs) code.
        match op {
            &BinOp::LAnd => {
                self.run(lhs, iseq, true)?;

                self.bytecode_gen.gen_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_gen.gen_jmp_if_false(0, iseq);

                self.bytecode_gen.gen_pop(iseq);

                self.run(rhs, iseq, true)?;

                let pos = iseq.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                self.bytecode_gen.gen_land(iseq);
                return Ok(());
            }
            &BinOp::LOr => {
                self.run(lhs, iseq, true)?;

                self.bytecode_gen.gen_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_gen.gen_jmp_if_false(0, iseq);

                let lhs_true_pos = iseq.len() as isize;
                self.bytecode_gen.gen_jmp(0, iseq);

                let pos = iseq.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                self.bytecode_gen.gen_pop(iseq);

                self.run(rhs, iseq, true)?;

                let pos = iseq.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_true_pos) as i32 - 5,
                    &mut iseq[lhs_true_pos as usize + 1..lhs_true_pos as usize + 5],
                );

                self.bytecode_gen.gen_lor(iseq);
                return Ok(());
            }
            _ => {}
        };

        self.run(lhs, iseq, true)?;
        self.run(rhs, iseq, true)?;

        match op {
            &BinOp::Add => self.bytecode_gen.gen_add(iseq),
            &BinOp::Sub => self.bytecode_gen.gen_sub(iseq),
            &BinOp::Mul => self.bytecode_gen.gen_mul(iseq),
            &BinOp::Div => self.bytecode_gen.gen_div(iseq),
            &BinOp::Rem => self.bytecode_gen.gen_rem(iseq),
            &BinOp::Eq => self.bytecode_gen.gen_eq(iseq),
            &BinOp::Ne => self.bytecode_gen.gen_ne(iseq),
            &BinOp::SEq => self.bytecode_gen.gen_seq(iseq),
            &BinOp::SNe => self.bytecode_gen.gen_sne(iseq),
            &BinOp::And => self.bytecode_gen.gen_and(iseq),
            &BinOp::Or => self.bytecode_gen.gen_or(iseq),
            &BinOp::Xor => self.bytecode_gen.gen_xor(iseq),
            &BinOp::Lt => self.bytecode_gen.gen_lt(iseq),
            &BinOp::Gt => self.bytecode_gen.gen_gt(iseq),
            &BinOp::Le => self.bytecode_gen.gen_le(iseq),
            &BinOp::Ge => self.bytecode_gen.gen_ge(iseq),
            &BinOp::Shl => self.bytecode_gen.gen_shl(iseq),
            &BinOp::Shr => self.bytecode_gen.gen_shr(iseq),
            &BinOp::ZFShr => self.bytecode_gen.gen_zfshr(iseq),
            _ => {}
        }

        Ok(())
    }

    pub fn run_ternary_op(
        &mut self,
        cond: &Node,
        then: &Node,
        else_: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.run(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(then, iseq, true)?;

        let then_end_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.run(else_, iseq, true)?;

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - then_end_pos) as i32 - 5,
            &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
        );

        self.bytecode_gen.gen_cond_op(iseq);

        Ok(())
    }

    pub fn run_assign(
        &mut self,
        dst: &Node,
        src: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        self.run(src, iseq, true)?;

        if use_value {
            self.bytecode_gen.gen_double(iseq);
        }

        self.assign_stack_top(dst, iseq)?;

        Ok(())
    }

    pub fn assign_stack_top(&mut self, dst: &Node, iseq: &mut ByteCode) -> Result<(), Error> {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.bytecode_gen.gen_set_value(name, iseq);
            }
            NodeBase::Member(ref parent, ref member) => {
                self.run(&*parent, iseq, true)?;
                self.bytecode_gen
                    .gen_push_const(Value::string(member.clone()), iseq);
                self.bytecode_gen.gen_set_member(iseq);
            }
            NodeBase::Index(ref parent, ref idx) => {
                self.run(&*parent, iseq, true)?;
                self.run(&*idx, iseq, true)?;
                self.bytecode_gen.gen_set_member(iseq);
            }
            _ => {
                return Err(Error::General {
                    msg: "error: invalid left hand expression".to_string(),
                    token_pos: dst.pos,
                });
            }
        }

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_call(
        &mut self,
        callee: &Node,
        args: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<(), Error> {
        for arg in args.iter().rev() {
            self.run(arg, iseq, true)?;
        }

        self.run(callee, iseq, true)?;

        self.bytecode_gen.gen_call(args.len() as u32, iseq);

        if !use_value {
            self.bytecode_gen.gen_pop(iseq);
        }

        Ok(())
    }
}

impl VMCodeGen {
    fn run_object_literal(
        &mut self,
        properties: &Vec<PropertyDefinition>,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        for property in properties {
            match property {
                PropertyDefinition::IdentifierReference(name) => {
                    self.run_identifier(name, iseq)?;
                    self.bytecode_gen
                        .gen_push_const(Value::string(name.clone()), iseq);
                }
                PropertyDefinition::Property(name, node) => {
                    self.run(&node, iseq, true)?;
                    self.bytecode_gen
                        .gen_push_const(Value::string(name.clone()), iseq);
                }
                _ => panic!(),
            }
        }

        self.bytecode_gen
            .gen_create_object(properties.len() as usize, iseq);

        Ok(())
    }

    fn run_array_literal(&mut self, elems: &Vec<Node>, iseq: &mut ByteCode) -> Result<(), Error> {
        for elem in elems.iter().rev() {
            self.run(elem, iseq, true)?;
        }

        self.bytecode_gen
            .gen_create_array(elems.len() as usize, iseq);

        Ok(())
    }
}

impl VMCodeGen {
    fn run_member(
        &mut self,
        parent: &Node,
        member: &String,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.run(parent, iseq, true)?;

        self.bytecode_gen
            .gen_push_const(Value::string(member.clone()), iseq);
        self.bytecode_gen.gen_get_member(iseq);

        Ok(())
    }

    fn run_index(&mut self, parent: &Node, idx: &Node, iseq: &mut ByteCode) -> Result<(), Error> {
        self.run(parent, iseq, true)?;

        self.run(idx, iseq, true)?;
        self.bytecode_gen.gen_get_member(iseq);

        Ok(())
    }

    fn run_identifier(&mut self, name: &String, iseq: &mut ByteCode) -> Result<(), Error> {
        self.bytecode_gen.gen_get_value(name, iseq);

        Ok(())
    }
}

impl VMCodeGen {
    fn replace_break_dsts(
        &mut self,
        label_name: &Option<String>,
        break_dst_pos: isize,
        iseq: &mut ByteCode,
    ) {
        let current_scope_level = self.state.scope_level;
        let current_try_level = self.state.try_level.len();
        self.state.break_inst_positions.retain(
            |(dst_label_name, inst_pos, scope_level, try_level, _)| {
                let x = VMCodeGen::label_name_predicate(label_name, &dst_label_name);
                if x {
                    VMCodeGen::replace_int32(
                        (break_dst_pos - inst_pos) as i32 - 5,
                        &mut iseq[*inst_pos as usize + 1..*inst_pos as usize + 5],
                    );
                    VMCodeGen::replace_uint32(
                        (scope_level - current_scope_level) as u32,
                        &mut iseq[*inst_pos as usize + 5..*inst_pos as usize + 9],
                    );
                    VMCodeGen::replace_uint32(
                        (try_level - current_try_level) as u32,
                        &mut iseq[*inst_pos as usize + 9..*inst_pos as usize + 13],
                    );
                }
                !x
            },
        );
    }

    fn replace_continue_dsts(
        &mut self,
        label_name: &Option<String>,
        continue_dst_pos: isize,
        iseq: &mut ByteCode,
    ) {
        let current_scope_level = self.state.scope_level;
        let current_try_level = self.state.try_level.len();
        self.state.continue_inst_positions.retain(
            |(dst_label_name, inst_pos, scope_level, try_level, _)| {
                let x = VMCodeGen::label_name_predicate(label_name, dst_label_name);
                if x {
                    VMCodeGen::replace_int32(
                        (continue_dst_pos - inst_pos) as i32 - 5,
                        &mut iseq[*inst_pos as usize + 1..*inst_pos as usize + 5],
                    );
                    VMCodeGen::replace_uint32(
                        (scope_level - current_scope_level) as u32,
                        &mut iseq[*inst_pos as usize + 5..*inst_pos as usize + 9],
                    );
                    VMCodeGen::replace_uint32(
                        (try_level - current_try_level) as u32,
                        &mut iseq[*inst_pos as usize + 9..*inst_pos as usize + 13],
                    );
                }
                !x
            },
        );
    }

    fn label_name_predicate(label_name: &Option<String>, dst_label_name: &Option<String>) -> bool {
        match dst_label_name.clone() {
            None => true,
            Some(dst_label_name) => match label_name.clone() {
                None => false,
                Some(label_name) => dst_label_name == *label_name,
            },
        }
    }

    fn replace_int32(n: i32, iseq: &mut [u8]) {
        iseq[3] = (n >> 24) as u8;
        iseq[2] = (n >> 16) as u8;
        iseq[1] = (n >> 8) as u8;
        iseq[0] = (n >> 0) as u8;
    }

    fn replace_uint32(n: u32, iseq: &mut [u8]) {
        iseq[3] = (n >> 24) as u8;
        iseq[2] = (n >> 16) as u8;
        iseq[1] = (n >> 8) as u8;
        iseq[0] = (n >> 0) as u8;
    }
}
