use bytecode_gen::{ByteCode, ByteCodeGen, VMInst};
use id;
use node::{BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp};
use vm::{
    callobj::{CallObject, CallObjectRef},
    value::Value,
};

#[derive(Clone, Debug)]
pub enum Error {
    General { msg: String, token_pos: usize },
    Unimplemented { msg: String },
}

#[derive(Clone, Debug)]
pub struct Jumps {
    global: JumpToGlobalLabel,
    local: Vec<JumpFromLoop>,
    loop_names: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct JumpToGlobalLabel {
    continue_inst_positions: Vec<(String, isize)>,
    break_inst_positions: Vec<(String, isize)>,
}

#[derive(Clone, Debug)]
pub struct JumpFromLoop {
    continue_inst_positions: Vec<isize>,
    break_inst_positions: Vec<isize>,
}

#[derive(Clone, Debug)]
pub enum FunctionHeaderInst {
    Closure(String, Value),
    DeclVar(String),
}

#[derive(Clone, Debug)]
pub struct VMCodeGen {
    pub global_varmap: CallObjectRef,
    pub func_header_info: Vec<Vec<FunctionHeaderInst>>,
    pub bytecode_gen: ByteCodeGen,
    pub labels: Jumps,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        let global = CallObject::new_global();
        VMCodeGen {
            global_varmap: global,
            func_header_info: vec![vec![]],
            bytecode_gen: ByteCodeGen::new(),
            labels: Jumps::new(),
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

        Ok(())
    }

    fn run(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> Result<(), Error> {
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

        match &node.base {
            &NodeBase::StatementList(ref node_list) => {
                self.run_statement_list(node_list, iseq, use_value)?
            }
            &NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.run_function_decl(name, params, &*body)?
            }
            &NodeBase::FunctionExpr(ref name, ref params, ref body) => {
                self.run_function_expr(name, params, &*body, iseq)?
            }
            &NodeBase::VarDecl(ref name, ref init) => self.run_var_decl(name, init, iseq)?,
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
            &NodeBase::Break(ref name) => self.run_break(name, iseq)?,
            &NodeBase::Continue(ref name) => self.run_continue(name, iseq)?,
            &NodeBase::Throw(ref val) => self.run_throw(val, iseq)?,
            &NodeBase::New(ref expr) => self.run_new_expr(&*expr, iseq)?,
            &NodeBase::Object(ref properties) => self.run_object_literal(properties, iseq)?,
            &NodeBase::Array(ref properties) => self.run_array_literal(properties, iseq)?,
            &NodeBase::Identifier(ref name) => self.run_identifier(name, iseq)?,
            &NodeBase::This => self.bytecode_gen.gen_push_this(iseq),
            &NodeBase::Arguments => self.bytecode_gen.gen_push_arguments(iseq),
            &NodeBase::Undefined => self.bytecode_gen.gen_push_undefined(iseq),
            &NodeBase::Null => self.bytecode_gen.gen_push_const(Value::null(), iseq),
            &NodeBase::String(ref s) => self
                .bytecode_gen
                .gen_push_const(Value::string(s.clone()), iseq),
            &NodeBase::Number(n) => self.bytecode_gen.gen_push_number(n, iseq),
            &NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, iseq),
            &NodeBase::Nope if use_value => {
                self.bytecode_gen.gen_push_const(Value::empty(), iseq);
            }
            _ => {}
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
}

impl VMCodeGen {
    fn set_function_header(&mut self, iseq: &mut ByteCode) {
        let mut section_callobj_set = vec![];
        let func_header_info = self.func_header_info.last_mut().unwrap();
        while let Some(header) = func_header_info.pop() {
            match header {
                FunctionHeaderInst::Closure(func_name, val) => {
                    self.bytecode_gen
                        .gen_decl_var(&func_name, &mut section_callobj_set);
                    self.bytecode_gen
                        .gen_push_const(val, &mut section_callobj_set);
                    self.bytecode_gen
                        .gen_update_parent_scope(&mut section_callobj_set);
                    self.bytecode_gen
                        .gen_set_value(&func_name, &mut section_callobj_set);
                }
                FunctionHeaderInst::DeclVar(var_name) => {
                    self.bytecode_gen
                        .gen_decl_var(&var_name, &mut section_callobj_set);
                }
            }
        }
        iseq.splice(1..1, section_callobj_set);
    }

    pub fn run_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
    ) -> Result<(), Error> {
        self.func_header_info.push(vec![]);

        let mut new_callobj = CallObject::new(unsafe { Value::object((*self.global_varmap).vals) });
        let mut func_iseq = vec![];

        self.bytecode_gen.gen_create_context(&mut func_iseq);

        self.run(body, &mut func_iseq, false)?;

        if !body.definitely_returns() {
            self.bytecode_gen.gen_push_undefined(&mut func_iseq);
            self.bytecode_gen.gen_return(&mut func_iseq);
        }

        new_callobj.params = params
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

        let val = Value::function(id::get_unique_id(), func_iseq.clone(), new_callobj);

        self.func_header_info.pop();

        self.func_header_info
            .last_mut()
            .unwrap()
            .push(FunctionHeaderInst::Closure(name.clone(), val));

        Ok(())
    }

    pub fn run_function_expr(
        &mut self,
        // TODO: _name should be used.
        _name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.func_header_info.push(vec![]);

        let mut new_callobj = CallObject::new(unsafe { Value::object((*self.global_varmap).vals) });

        let mut func_iseq = vec![];

        self.bytecode_gen.gen_create_context(&mut func_iseq);

        self.run(body, &mut func_iseq, false)?;

        if !body.definitely_returns() {
            self.bytecode_gen.gen_push_undefined(&mut func_iseq);
            self.bytecode_gen.gen_return(&mut func_iseq);
        }

        new_callobj.params = params
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

        let val = Value::function(id::get_unique_id(), func_iseq.clone(), new_callobj);

        self.bytecode_gen.gen_push_const(val, iseq);
        self.bytecode_gen.gen_update_parent_scope(iseq);

        self.func_header_info.pop();

        Ok(())
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

        self.bytecode_gen.gen_return(iseq);

        Ok(())
    }

    pub fn run_throw(
        &mut self,
        val: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.run(val, iseq, true)?;
        self.bytecode_gen.gen_throw(iseq);

        Ok(())
    }
}

impl VMCodeGen {
    pub fn run_break(&mut self, name: &Option<String>, iseq: &mut ByteCode) -> Result<(), Error> {
        let break_inst_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);

        if let Some(name) = name {
            self.labels
                .global
                .break_inst_positions
                .push((name.clone(), break_inst_pos));
        } else {
            self.labels
                .local
                .last_mut()
                .unwrap()
                .break_inst_positions
                .push(break_inst_pos);
        }

        Ok(())
    }

    pub fn run_continue(
        &mut self,
        name: &Option<String>,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        let continue_inst_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);

        if let Some(name) = name {
            self.labels
                .global
                .continue_inst_positions
                .push((name.clone(), continue_inst_pos));
        } else {
            self.labels
                .local
                .last_mut()
                .unwrap()
                .continue_inst_positions
                .push(continue_inst_pos);
        }

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
    ) -> Result<(), Error> {
        if let &Some(ref init) = init {
            self.run(&*init, iseq, true)?;
        } else {
            self.bytecode_gen.gen_push_undefined(iseq);
        }

        self.func_header_info
            .last_mut()
            .unwrap()
            .push(FunctionHeaderInst::DeclVar(name.clone()));
        self.bytecode_gen.gen_set_value(name, iseq);

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
        let name = self.labels.loop_names.pop();

        let pos1 = iseq.len() as isize;
        self.labels.make_new_local();

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
        self.labels
            .cur_local()
            .replace_break_dsts(&mut self.bytecode_gen, break_pos, iseq);

        self.labels
            .cur_local()
            .replace_continue_dsts(&mut self.bytecode_gen, pos1, iseq);

        if let Some(name) = name {
            self.labels
                .global
                .replace_continue_dsts(&mut self.bytecode_gen, &name, pos1, iseq);
        }

        let pos2 = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos2 - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.labels.pop_local();

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
        let name = self.labels.loop_names.pop();

        self.run(init, iseq, false)?;

        let pos = iseq.len() as isize;
        self.labels.make_new_local();

        self.bytecode_gen.gen_loop_start(iseq);

        self.run(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq, false)?;

        let continue_pos = iseq.len() as isize;
        self.labels
            .cur_local()
            .replace_continue_dsts(&mut self.bytecode_gen, continue_pos, iseq);

        if let Some(name) = name {
            self.labels.global.replace_continue_dsts(
                &mut self.bytecode_gen,
                &name,
                continue_pos,
                iseq,
            );
        }

        self.run(step, iseq, false)?;

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp((pos - loop_pos) as i32 - 5, iseq);

        self.bytecode_gen.replace_int32(
            iseq.len() as i32 - pos as i32,
            &mut iseq[pos as usize + 1..pos as usize + 5],
        );

        let break_pos = iseq.len() as isize;
        self.labels
            .cur_local()
            .replace_break_dsts(&mut self.bytecode_gen, break_pos, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.labels.pop_local();

        Ok(())
    }

    pub fn run_label(
        &mut self,
        name: &String,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> Result<(), Error> {
        self.labels.loop_names.push(name.clone());

        self.run(body, iseq, false)?;

        let break_label_pos = iseq.len() as isize;
        self.labels
            .global
            .replace_break_dsts(&mut self.bytecode_gen, name, break_label_pos, iseq);

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

// Manage Breaks and Continues

impl Jumps {
    pub fn new() -> Self {
        Jumps {
            global: JumpToGlobalLabel::new(),
            local: vec![],
            loop_names: vec![],
        }
    }

    pub fn make_new_local(&mut self) {
        self.local.push(JumpFromLoop::new());
    }

    pub fn pop_local(&mut self) {
        self.local.pop();
    }

    pub fn cur_local(&mut self) -> &mut JumpFromLoop {
        self.local.last_mut().unwrap()
    }
}

impl JumpToGlobalLabel {
    pub fn new() -> Self {
        JumpToGlobalLabel {
            continue_inst_positions: vec![],
            break_inst_positions: vec![],
        }
    }

    fn replace_break_dsts(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        label_name: &String,
        break_dst_pos: isize,
        iseq: &mut ByteCode,
    ) {
        self.break_inst_positions
            .retain(|(dst_label_name, inst_pos)| {
                let x = dst_label_name == label_name;
                if x {
                    bytecode_gen.replace_int32(
                        (break_dst_pos - inst_pos) as i32 - 5,
                        &mut iseq[*inst_pos as usize + 1..*inst_pos as usize + 5],
                    );
                }
                !x
            });
    }

    fn replace_continue_dsts(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        label_name: &String,
        continue_dst_pos: isize,
        iseq: &mut ByteCode,
    ) {
        self.continue_inst_positions
            .retain(|(dst_label_name, inst_pos)| {
                let x = dst_label_name == label_name;
                if x {
                    bytecode_gen.replace_int32(
                        (continue_dst_pos - inst_pos) as i32 - 5,
                        &mut iseq[*inst_pos as usize + 1..*inst_pos as usize + 5],
                    );
                }
                !x
            });
    }
}

impl JumpFromLoop {
    pub fn new() -> Self {
        JumpFromLoop {
            continue_inst_positions: vec![],
            break_inst_positions: vec![],
        }
    }

    fn replace_break_dsts(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        break_dst_pos: isize,
        iseq: &mut ByteCode,
    ) {
        for inst_pos in &self.break_inst_positions {
            bytecode_gen.replace_int32(
                (break_dst_pos - inst_pos) as i32 - 5,
                &mut iseq[*inst_pos as usize + 1..*inst_pos as usize + 5],
            );
        }
        self.break_inst_positions.clear();
    }

    fn replace_continue_dsts(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        continue_dst_pos: isize,
        iseq: &mut ByteCode,
    ) {
        for inst_pos in &self.continue_inst_positions {
            bytecode_gen.replace_int32(
                (continue_dst_pos - inst_pos) as i32 - 5,
                &mut iseq[*inst_pos as usize + 1..*inst_pos as usize + 5],
            );
        }
        self.continue_inst_positions.clear();
    }
}
