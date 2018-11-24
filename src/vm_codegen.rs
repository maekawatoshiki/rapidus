use bytecode_gen::{ByteCode, ByteCodeGen, VMInst};
use gc;
use id;
use node::{BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp};
use vm::{
    callobj::{CallObject, CallObjectRef}, value::Value,
};

use std::collections::HashMap;
use std::ffi::CString;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub params: FormalParameters,
    pub iseq: ByteCode,
}

impl FunctionInfo {
    pub fn new(name: String, params: FormalParameters, iseq: ByteCode) -> FunctionInfo {
        FunctionInfo {
            name: name,
            params: params,
            iseq: iseq,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Labels {
    continue_jmp_list: Vec<isize>,
    break_jmp_list: Vec<isize>,
}

impl Labels {
    pub fn new() -> Labels {
        Labels {
            continue_jmp_list: vec![],
            break_jmp_list: vec![],
        }
    }

    fn replace_break_jmps(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        iseq: &mut ByteCode,
        break_label_pos: isize,
    ) {
        for jmp_pos in &self.break_jmp_list {
            bytecode_gen.replace_int32(
                (break_label_pos - jmp_pos) as i32 - 5,
                &mut iseq[*jmp_pos as usize + 1..*jmp_pos as usize + 5],
            );
        }
        self.break_jmp_list.clear();
    }

    fn replace_continue_jmps(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        iseq: &mut ByteCode,
        continue_label_pos: isize,
    ) {
        for jmp_pos in &self.continue_jmp_list {
            bytecode_gen.replace_int32(
                (continue_label_pos - jmp_pos) as i32 - 5,
                &mut iseq[*jmp_pos as usize + 1..*jmp_pos as usize + 5],
            );
        }
        self.continue_jmp_list.clear();
    }
}

#[derive(Clone, Debug)]
pub struct VMCodeGen {
    pub global_varmap: CallObjectRef,
    pub cur_callobj: CallObjectRef,
    pub functions: HashMap<String, FunctionInfo>,
    pub bytecode_gen: ByteCodeGen,
    pub labels: Vec<Labels>,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        let global = CallObject::new_global();
        VMCodeGen {
            global_varmap: global,
            cur_callobj: global,
            functions: HashMap::new(),
            bytecode_gen: ByteCodeGen::new(),
            labels: vec![Labels::new()],
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) {
        self.bytecode_gen.gen_create_context(iseq);

        self.run(node, iseq, use_value);

        self.bytecode_gen.gen_end(iseq);

        for (
            _,
            FunctionInfo {
                name,
                params,
                iseq: func_iseq,
            },
        ) in &self.functions
        {
            let val = Value::function(id::get_unique_id(), func_iseq.clone(), {
                let mut callobj =
                    CallObject::new(unsafe { Value::object((*self.global_varmap).vals.clone()) });
                callobj.params = params
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
                callobj.parent = Some(self.global_varmap.clone());
                callobj
            });

            unsafe {
                (*self.global_varmap).set_value(name.clone(), val.clone());
            }
        }
    }

    fn run(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) {
        if let Some(constant) = node.base.fold_num_consts() {
            match constant {
                NodeBase::String(ref s) => self
                    .bytecode_gen
                    .gen_push_const(Value::string(CString::new(s.as_str()).unwrap()), iseq),
                NodeBase::Number(n) => self.bytecode_gen.gen_push_number(n, iseq),
                NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, iseq),
                _ => unreachable!(),
            }
            return;
        }

        match &node.base {
            &NodeBase::StatementList(ref node_list) => {
                self.run_statement_list(node_list, iseq, use_value)
            }
            &NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.run_function_decl(name, params, &*body)
            }
            &NodeBase::FunctionExpr(ref name, ref params, ref body) => {
                self.run_function_expr(name, params, &*body, iseq)
            }
            &NodeBase::VarDecl(ref name, ref init) => {
                self.run_var_decl(name, init, iseq);
            }
            &NodeBase::If(ref cond, ref then_, ref else_) => {
                self.run_if(&*cond, &*then_, &*else_, iseq)
            }
            &NodeBase::While(ref cond, ref body) => self.run_while(&*cond, &*body, iseq),
            &NodeBase::For(ref init, ref cond, ref step, ref body) => {
                self.run_for(&*init, &*cond, &*step, &*body, iseq)
            }
            &NodeBase::Assign(ref dst, ref src) => self.run_assign(&*dst, &*src, iseq, use_value),
            &NodeBase::UnaryOp(ref expr, ref op) => self.run_unary_op(&*expr, op, iseq, use_value),
            &NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.run_binary_op(&*lhs, &*rhs, op, iseq)
            }
            &NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                self.run_ternary_op(&*cond, &*then, &*else_, iseq)
            }
            &NodeBase::Call(ref callee, ref args) => self.run_call(&*callee, args, iseq, use_value),
            &NodeBase::Member(ref parent, ref member) => self.run_member(&*parent, member, iseq),
            &NodeBase::Index(ref parent, ref idx) => self.run_index(&*parent, &*idx, iseq),
            &NodeBase::Return(ref val) => self.run_return(val, iseq),
            &NodeBase::Break => self.run_break(iseq),
            &NodeBase::Continue => self.run_continue(iseq),
            &NodeBase::New(ref expr) => self.run_new_expr(&*expr, iseq),
            &NodeBase::Object(ref properties) => self.run_object_literal(properties, iseq),
            &NodeBase::Array(ref properties) => self.run_array_literal(properties, iseq),
            &NodeBase::Identifier(ref name) => self.run_identifier(name, iseq),
            &NodeBase::This => self.bytecode_gen.gen_push_this(iseq),
            &NodeBase::Arguments => self.bytecode_gen.gen_push_arguments(iseq),
            &NodeBase::Undefined => self.bytecode_gen.gen_push_undefined(iseq),
            &NodeBase::String(ref s) => self
                .bytecode_gen
                .gen_push_const(Value::string(CString::new(s.as_str()).unwrap()), iseq),
            &NodeBase::Number(n) => self.bytecode_gen.gen_push_number(n, iseq),
            &NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, iseq),
            &NodeBase::Nope if use_value => {
                self.bytecode_gen.gen_push_const(Value::empty(), iseq);
            }

            _ => {}
        }
    }
}

impl VMCodeGen {
    pub fn run_statement_list(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) {
        for node in node_list {
            self.run(node, iseq, use_value)
        }
    }
}

impl VMCodeGen {
    pub fn run_function_decl(&mut self, name: &String, params: &FormalParameters, body: &Node) {
        let parent_callobj = self.cur_callobj;
        let mut new_callobj =
            CallObject::new(unsafe { Value::object((*self.global_varmap).vals.clone()) });
        new_callobj.parent = Some(parent_callobj);
        self.cur_callobj = gc::new(new_callobj);

        let mut func_iseq = vec![];

        self.bytecode_gen.gen_create_context(&mut func_iseq);

        self.run(body, &mut func_iseq, false);

        match func_iseq.last() {
            Some(&VMInst::RETURN) => {}
            _ => {
                self.bytecode_gen
                    .gen_push_const(Value::undefined(), &mut func_iseq);
                self.bytecode_gen.gen_return(&mut func_iseq);
            }
        }

        unsafe {
            (*self.cur_callobj).params = params
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
        }

        let val = Value::function(id::get_unique_id(), func_iseq.clone(), unsafe {
            (*self.cur_callobj).clone()
        });

        unsafe { (*parent_callobj).set_value(name.clone(), val.clone()) }

        self.cur_callobj = parent_callobj;
    }

    pub fn run_function_expr(
        &mut self,
        // TODO: _name should be used.
        _name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
    ) {
        let parent_callobj = self.cur_callobj;
        let mut new_callobj =
            CallObject::new(unsafe { Value::object((*self.global_varmap).vals.clone()) });
        new_callobj.parent = Some(parent_callobj);
        self.cur_callobj = gc::new(new_callobj);

        let mut func_iseq = vec![];

        self.bytecode_gen.gen_create_context(&mut func_iseq);

        self.run(body, &mut func_iseq, false);

        match func_iseq.last() {
            Some(&VMInst::RETURN) => {}
            _ => {
                self.bytecode_gen
                    .gen_push_const(Value::undefined(), &mut func_iseq);
                self.bytecode_gen.gen_return(&mut func_iseq);
            }
        }

        unsafe {
            (*self.cur_callobj).params = params
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
        }

        let val = Value::function(id::get_unique_id(), func_iseq.clone(), unsafe {
            (*self.cur_callobj).clone()
        });

        self.bytecode_gen.gen_push_const(val, iseq);

        self.cur_callobj = parent_callobj;
    }

    pub fn run_return(&mut self, val: &Option<Box<Node>>, iseq: &mut ByteCode) {
        if let &Some(ref val) = val {
            self.run(&*val, iseq, true)
        } else {
            self.bytecode_gen.gen_push_const(Value::undefined(), iseq);
        }
        self.bytecode_gen.gen_return(iseq);
    }
}

impl VMCodeGen {
    pub fn run_break(&mut self, iseq: &mut ByteCode) {
        let break_jmp_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);
        self.labels
            .last_mut()
            .unwrap()
            .break_jmp_list
            .push(break_jmp_pos);
    }

    pub fn run_continue(&mut self, iseq: &mut ByteCode) {
        let continue_jmp_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);
        self.labels
            .last_mut()
            .unwrap()
            .continue_jmp_list
            .push(continue_jmp_pos);
    }
}

impl VMCodeGen {
    pub fn run_new_expr(&mut self, expr: &Node, iseq: &mut ByteCode) {
        self.run(expr, iseq, true);
        let len = iseq.len();
        if iseq[len - 1 - 4] == VMInst::CALL {
            iseq[len - 1 - 4] = VMInst::CONSTRUCT;
        } else {
            unreachable!()
        }
    }
}

impl VMCodeGen {
    pub fn run_var_decl(&mut self, name: &String, init: &Option<Box<Node>>, iseq: &mut ByteCode) {
        if let &Some(ref init) = init {
            self.run(&*init, iseq, true);
        } else {
            self.bytecode_gen.gen_push_const(Value::undefined(), iseq);
        }
        self.bytecode_gen.gen_decl_var(name, iseq);
    }
}

impl VMCodeGen {
    pub fn run_if(&mut self, cond: &Node, then_: &Node, else_: &Node, iseq: &mut ByteCode) {
        self.run(cond, iseq, true);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(then_, iseq, false);

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

            self.run(else_, iseq, false);

            let pos = iseq.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - then_end_pos) as i32 - 5,
                &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
            );
        }
    }

    pub fn run_while(&mut self, cond: &Node, body: &Node, iseq: &mut ByteCode) {
        let pos1 = iseq.len() as isize;
        self.labels.push(Labels::new());

        self.bytecode_gen.gen_loop_start(iseq);

        self.run(cond, iseq, true);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq, false);

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen
            .gen_jmp((pos1 - loop_pos) as i32 - 5, iseq);

        self.bytecode_gen.replace_int32(
            iseq.len() as i32,
            &mut iseq[pos1 as usize + 1..pos1 as usize + 5],
        );

        let break_label_pos = iseq.len() as isize;
        self.labels.last_mut().unwrap().replace_break_jmps(
            &mut self.bytecode_gen,
            iseq,
            break_label_pos,
        );
        self.labels
            .last_mut()
            .unwrap()
            .replace_continue_jmps(&mut self.bytecode_gen, iseq, pos1);
        self.labels.pop();

        let pos2 = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos2 - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );
    }

    pub fn run_for(
        &mut self,
        init: &Node,
        cond: &Node,
        step: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) {
        self.run(init, iseq, false);

        let pos = iseq.len() as isize;
        self.labels.push(Labels::new());

        self.bytecode_gen.gen_loop_start(iseq);

        self.run(cond, iseq, true);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq, false);

        let continue_label_pos = iseq.len() as isize;
        self.labels.last_mut().unwrap().replace_continue_jmps(
            &mut self.bytecode_gen,
            iseq,
            continue_label_pos,
        );
        self.run(step, iseq, false);

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp((pos - loop_pos) as i32 - 5, iseq);

        self.bytecode_gen.replace_int32(
            iseq.len() as i32,
            &mut iseq[pos as usize + 1..pos as usize + 5],
        );

        let break_label_pos = iseq.len() as isize;
        self.labels.last_mut().unwrap().replace_break_jmps(
            &mut self.bytecode_gen,
            iseq,
            break_label_pos,
        );
        self.labels.pop();

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );
    }
}

impl VMCodeGen {
    pub fn run_unary_op(
        &mut self,
        expr: &Node,
        op: &UnaryOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) {
        self.run(expr, iseq, true);
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
                self.assign_stack_top(expr, iseq)
            }
            &UnaryOp::PoInc => {
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_add(iseq);
                self.assign_stack_top(expr, iseq)
            }
            &UnaryOp::PrDec => {
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_sub(iseq);
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.assign_stack_top(expr, iseq)
            }
            &UnaryOp::PoDec => {
                if use_value {
                    self.bytecode_gen.gen_double(iseq);
                }
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_sub(iseq);
                self.assign_stack_top(expr, iseq)
            }
            _ => unimplemented!(),
        }
    }

    pub fn run_binary_op(&mut self, lhs: &Node, rhs: &Node, op: &BinOp, iseq: &mut ByteCode) {
        // Following code has influence on JIT(src/jit.rs) code.
        match op {
            &BinOp::LAnd => {
                self.run(lhs, iseq, true);

                self.bytecode_gen.gen_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_gen.gen_jmp_if_false(0, iseq);

                self.bytecode_gen.gen_pop(iseq);

                self.run(rhs, iseq, true);

                let pos = iseq.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                self.bytecode_gen.gen_land(iseq);
                return;
            }
            &BinOp::LOr => {
                self.run(lhs, iseq, true);

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

                self.run(rhs, iseq, true);

                let pos = iseq.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_true_pos) as i32 - 5,
                    &mut iseq[lhs_true_pos as usize + 1..lhs_true_pos as usize + 5],
                );

                self.bytecode_gen.gen_lor(iseq);
                return;
            }
            _ => {}
        };

        self.run(lhs, iseq, true);
        self.run(rhs, iseq, true);
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
    }

    pub fn run_ternary_op(&mut self, cond: &Node, then: &Node, else_: &Node, iseq: &mut ByteCode) {
        self.run(cond, iseq, true);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(then, iseq, true);

        let then_end_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.run(else_, iseq, true);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - then_end_pos) as i32 - 5,
            &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
        );

        self.bytecode_gen.gen_cond_op(iseq);
    }

    pub fn run_assign(&mut self, dst: &Node, src: &Node, iseq: &mut ByteCode, use_value: bool) {
        self.run(src, iseq, true);

        if use_value {
            self.bytecode_gen.gen_double(iseq);
        }

        self.assign_stack_top(dst, iseq);
    }

    pub fn assign_stack_top(&mut self, dst: &Node, iseq: &mut ByteCode) {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.bytecode_gen.gen_set_name(name, iseq);
            }
            NodeBase::Member(ref parent, ref member) => {
                self.run(&*parent, iseq, true);
                self.bytecode_gen
                    .gen_push_const(Value::string(CString::new(member.as_str()).unwrap()), iseq);
                self.bytecode_gen.gen_set_member(iseq);
            }
            NodeBase::Index(ref parent, ref idx) => {
                self.run(&*parent, iseq, true);
                self.run(&*idx, iseq, true);
                self.bytecode_gen.gen_set_member(iseq);
            }
            _ => unimplemented!(),
        }
    }
}

impl VMCodeGen {
    pub fn run_call(
        &mut self,
        callee: &Node,
        args: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) {
        for arg in args.iter().rev() {
            self.run(arg, iseq, true);
        }

        self.run(callee, iseq, true);

        self.bytecode_gen.gen_call(args.len() as u32, iseq);

        if !use_value {
            self.bytecode_gen.gen_pop(iseq);
        }
    }
}

impl VMCodeGen {
    fn run_object_literal(&mut self, properties: &Vec<PropertyDefinition>, iseq: &mut ByteCode) {
        for property in properties {
            match property {
                PropertyDefinition::IdentifierReference(name) => {
                    self.run_identifier(name, iseq);
                    self.bytecode_gen
                        .gen_push_const(Value::string(CString::new(name.as_str()).unwrap()), iseq);
                }
                PropertyDefinition::Property(name, node) => {
                    self.run(&node, iseq, true);
                    self.bytecode_gen
                        .gen_push_const(Value::string(CString::new(name.as_str()).unwrap()), iseq);
                }
            }
        }

        self.bytecode_gen
            .gen_create_object(properties.len() as usize, iseq);
    }

    fn run_array_literal(&mut self, elems: &Vec<Node>, iseq: &mut ByteCode) {
        for elem in elems.iter().rev() {
            self.run(elem, iseq, true);
        }

        self.bytecode_gen
            .gen_create_array(elems.len() as usize, iseq);
    }
}

impl VMCodeGen {
    fn run_member(&mut self, parent: &Node, member: &String, iseq: &mut ByteCode) {
        self.run(parent, iseq, true);

        self.bytecode_gen
            .gen_push_const(Value::string(CString::new(member.as_str()).unwrap()), iseq);
        self.bytecode_gen.gen_get_member(iseq);
    }

    fn run_index(&mut self, parent: &Node, idx: &Node, iseq: &mut ByteCode) {
        self.run(parent, iseq, true);

        self.run(idx, iseq, true);
        self.bytecode_gen.gen_get_member(iseq);
    }

    fn run_identifier(&mut self, name: &String, iseq: &mut ByteCode) {
        self.bytecode_gen.gen_get_name(name, iseq);
    }
}
