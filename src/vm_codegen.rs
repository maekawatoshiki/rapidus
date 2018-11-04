use bytecode_gen::{ByteCode, ByteCodeGen, VMInst};
use id;
use node::{BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp};
use vm::{new_value_function, CallObject, CallObjectRef, Value};

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
    pub functions: HashMap<String, FunctionInfo>,
    pub bytecode_gen: ByteCodeGen,
    pub labels: Vec<Labels>,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: CallObject::new_global(),
            functions: HashMap::new(),
            bytecode_gen: ByteCodeGen::new(),
            labels: vec![Labels::new()],
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, iseq: &mut ByteCode) {
        self.bytecode_gen.gen_create_context(iseq);

        self.run(node, iseq);

        self.bytecode_gen.gen_end(iseq);

        // let mut function_value_list = HashMap::new();
        //
        // {
        //     function_value_list.insert("console".to_string(), {
        //         let mut map = HashMap::new();
        //         map.insert(
        //             "log".to_string(),
        //             Value::BuiltinFunction(builtin::CONSOLE_LOG, CallObject::new(Value::Undefined)),
        //         );
        //         Value::Object(Rc::new(RefCell::new(map)))
        //     });
        //
        //     function_value_list.insert("process".to_string(), {
        //         let mut map = HashMap::new();
        //         map.insert("stdout".to_string(), {
        //             let mut map = HashMap::new();
        //             map.insert(
        //                 "write".to_string(),
        //                 Value::BuiltinFunction(
        //                     builtin::PROCESS_STDOUT_WRITE,
        //                     CallObject::new(Value::Undefined),
        //                 ),
        //             );
        //             Value::Object(Rc::new(RefCell::new(map)))
        //         });
        //         Value::Object(Rc::new(RefCell::new(map)))
        //     });
        //
        //     function_value_list.insert("Math".to_string(), {
        //         let mut map = HashMap::new();
        //         map.insert(
        //             "floor".to_string(),
        //             Value::BuiltinFunction(builtin::MATH_FLOOR, CallObject::new(Value::Undefined)),
        //         );
        //         map.insert(
        //             "random".to_string(),
        //             Value::BuiltinFunction(builtin::MATH_RANDOM, CallObject::new(Value::Undefined)),
        //         );
        //         map.insert(
        //             "pow".to_string(),
        //             Value::BuiltinFunction(builtin::MATH_POW, CallObject::new(Value::Undefined)),
        //         );
        //         Value::Object(Rc::new(RefCell::new(map)))
        //     });
        // }

        for (
            _,
            FunctionInfo {
                name,
                params,
                iseq: func_iseq,
            },
        ) in &self.functions
        {
            let val = new_value_function(id::get_unique_id(), func_iseq.clone(), {
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

        // let mut i = 0;
        // while i < iseq.len() {
        //     let inst_size = VMInst::get_inst_size(iseq[i])
        //         .unwrap_or_else(|| panic!("Illegal VM Instruction occurred"));
        //     match iseq[i] {
        //         VMInst::GET_NAME => {
        //             let id = iseq[i + 1] as i32
        //                 + ((iseq[i + 2] as i32) << 8)
        //                 + ((iseq[i + 3] as i32) << 16)
        //                 + ((iseq[i + 4] as i32) << 24);
        //             if let Some(val) = function_value_list
        //                 .get(self.bytecode_gen.const_table.string[id as usize].as_str())
        //             {
        //                 match val {
        //                     Value::NeedThis(callee) => {
        //                         iseq[i] = VMInst::PUSH_CONST;
        //                         let id = self.bytecode_gen.const_table.value.len();
        //                         self.bytecode_gen
        //                             .const_table
        //                             .value
        //                             .push(Value::NeedThis(callee.clone()));
        //                         self.bytecode_gen
        //                             .replace_int32(id as i32, &mut iseq[i + 1..i + 5]);
        //                     }
        //                     _ => {
        //                         iseq[i] = VMInst::PUSH_CONST;
        //                         let id = self.bytecode_gen.const_table.value.len();
        //                         self.bytecode_gen.const_table.value.push(val.clone());
        //                         self.bytecode_gen
        //                             .replace_int32(id as i32, &mut iseq[i + 1..i + 5]);
        //                     }
        //                 }
        //             }
        //             i += inst_size
        //         }
        //         _ => i += inst_size,
        //     }
        // }
    }

    fn run(&mut self, node: &Node, iseq: &mut ByteCode) {
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
            &NodeBase::StatementList(ref node_list) => self.run_statement_list(node_list, iseq),
            &NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.run_function_decl(name, params, &*body)
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
            &NodeBase::Assign(ref dst, ref src) => self.run_assign(&*dst, &*src, iseq),
            &NodeBase::UnaryOp(ref expr, ref op) => self.run_unary_op(&*expr, op, iseq),
            &NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.run_binary_op(&*lhs, &*rhs, op, iseq)
            }
            &NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                self.run_ternary_op(&*cond, &*then, &*else_, iseq)
            }
            &NodeBase::Call(ref callee, ref args) => self.run_call(&*callee, args, iseq),
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
            &NodeBase::SetCurCallObj(ref name) => {
                self.bytecode_gen.gen_get_name(name, iseq);
                self.bytecode_gen.gen_set_cur_callobj(iseq);
            }
            _ => {}
        }
    }
}

impl VMCodeGen {
    pub fn run_statement_list(&mut self, node_list: &Vec<Node>, iseq: &mut ByteCode) {
        for node in node_list {
            self.run(node, iseq)
        }
    }
}

impl VMCodeGen {
    pub fn run_function_decl(&mut self, name: &String, params: &FormalParameters, body: &Node) {
        let mut func_iseq = vec![];

        self.bytecode_gen.gen_create_context(&mut func_iseq);

        // TODO: Implement Rest Parameter ASAP

        self.run(body, &mut func_iseq);

        match func_iseq.last() {
            Some(&VMInst::RETURN) => {}
            _ => {
                self.bytecode_gen
                    .gen_push_const(Value::undefined(), &mut func_iseq);
                self.bytecode_gen.gen_return(&mut func_iseq);
            }
        }

        self.functions.insert(
            name.clone(),
            FunctionInfo::new(name.clone(), params.clone(), func_iseq),
        );
    }

    pub fn run_return(&mut self, val: &Option<Box<Node>>, iseq: &mut ByteCode) {
        if let &Some(ref val) = val {
            self.run(&*val, iseq)
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
        self.run(expr, iseq);
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
            self.run(&*init, iseq);
        } else {
            self.bytecode_gen.gen_push_const(Value::undefined(), iseq);
        }
        self.bytecode_gen.gen_decl_var(name, iseq);
    }
}

impl VMCodeGen {
    pub fn run_if(&mut self, cond: &Node, then_: &Node, else_: &Node, iseq: &mut ByteCode) {
        self.run(cond, iseq);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(then_, iseq);

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

            self.run(else_, iseq);

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

        self.run(cond, iseq);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq);

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen
            .gen_jmp((pos1 - loop_pos) as i32 - 5, iseq);

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
        self.run(init, iseq);

        let pos = iseq.len() as isize;
        self.labels.push(Labels::new());

        self.run(cond, iseq);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(body, iseq);

        let continue_label_pos = iseq.len() as isize;
        self.labels.last_mut().unwrap().replace_continue_jmps(
            &mut self.bytecode_gen,
            iseq,
            continue_label_pos,
        );
        self.run(step, iseq);

        let loop_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp((pos - loop_pos) as i32 - 5, iseq);

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
    pub fn run_unary_op(&mut self, expr: &Node, op: &UnaryOp, iseq: &mut ByteCode) {
        self.run(expr, iseq);
        match op {
            &UnaryOp::Plus => self.bytecode_gen.gen_posi(iseq),
            &UnaryOp::Minus => self.bytecode_gen.gen_neg(iseq),
            &UnaryOp::Not => self.bytecode_gen.gen_lnot(iseq),
            &UnaryOp::PrInc => {
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_add(iseq);
                self.bytecode_gen.gen_double(iseq);
                self.assign_stack_top(expr, iseq)
            }
            &UnaryOp::PoInc => {
                self.bytecode_gen.gen_double(iseq);
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_add(iseq);
                self.assign_stack_top(expr, iseq)
            }
            &UnaryOp::PrDec => {
                self.bytecode_gen.gen_push_int8(1, iseq);
                self.bytecode_gen.gen_sub(iseq);
                self.bytecode_gen.gen_double(iseq);
                self.assign_stack_top(expr, iseq)
            }
            &UnaryOp::PoDec => {
                self.bytecode_gen.gen_double(iseq);
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
                self.run(lhs, iseq);

                self.bytecode_gen.gen_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_gen.gen_jmp_if_false(0, iseq);

                self.bytecode_gen.gen_pop(iseq);

                self.run(rhs, iseq);

                let pos = iseq.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                self.bytecode_gen.gen_land(iseq);
                return;
            }
            &BinOp::LOr => {
                self.run(lhs, iseq);

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

                self.run(rhs, iseq);

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

        self.run(lhs, iseq);
        self.run(rhs, iseq);
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
        self.run(cond, iseq);

        let cond_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, iseq);

        self.run(then, iseq);

        let then_end_pos = iseq.len() as isize;
        self.bytecode_gen.gen_jmp(0, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.run(else_, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - then_end_pos) as i32 - 5,
            &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
        );

        self.bytecode_gen.gen_cond_op(iseq);
    }

    pub fn run_assign(&mut self, dst: &Node, src: &Node, iseq: &mut ByteCode) {
        self.run(src, iseq);
        self.assign_stack_top(dst, iseq);
    }

    pub fn assign_stack_top(&mut self, dst: &Node, iseq: &mut ByteCode) {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.bytecode_gen.gen_set_name(name, iseq);
            }
            NodeBase::Member(ref parent, ref member) => {
                self.run(&*parent, iseq);
                self.bytecode_gen
                    .gen_push_const(Value::string(CString::new(member.as_str()).unwrap()), iseq);
                self.bytecode_gen.gen_set_member(iseq);
            }
            NodeBase::Index(ref parent, ref idx) => {
                self.run(&*parent, iseq);
                self.run(&*idx, iseq);
                self.bytecode_gen.gen_set_member(iseq);
            }
            _ => unimplemented!(),
        }
    }
}

impl VMCodeGen {
    pub fn run_call(&mut self, callee: &Node, args: &Vec<Node>, iseq: &mut ByteCode) {
        for arg in args.iter().rev() {
            self.run(arg, iseq);
        }

        self.run(callee, iseq);

        self.bytecode_gen.gen_call(args.len() as u32, iseq);
    }
}

impl VMCodeGen {
    fn run_object_literal(&mut self, properties: &Vec<PropertyDefinition>, iseq: &mut ByteCode) {
        for property in properties {
            match property {
                PropertyDefinition::IdentifierReference(_) => unimplemented!(),
                PropertyDefinition::Property(name, node) => {
                    self.run(&node, iseq);
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
            self.run(elem, iseq);
        }

        self.bytecode_gen
            .gen_create_array(elems.len() as usize, iseq);
    }
}

impl VMCodeGen {
    fn run_member(&mut self, parent: &Node, member: &String, iseq: &mut ByteCode) {
        self.run(parent, iseq);

        self.bytecode_gen
            .gen_push_const(Value::string(CString::new(member.as_str()).unwrap()), iseq);
        self.bytecode_gen.gen_get_member(iseq);
    }

    fn run_index(&mut self, parent: &Node, idx: &Node, iseq: &mut ByteCode) {
        self.run(parent, iseq);

        self.run(idx, iseq);
        self.bytecode_gen.gen_get_member(iseq);
    }

    fn run_identifier(&mut self, name: &String, iseq: &mut ByteCode) {
        self.bytecode_gen.gen_get_name(name, iseq);
    }
}

// #[test]
// fn binaryop() {
//     use parser;
//     for (op_s, op_i) in vec![
//         ("+", Inst::Add),
//         ("-", Inst::Sub),
//         ("*", Inst::Mul),
//         ("/", Inst::Div),
//         ("%", Inst::Rem),
//         ("<", Inst::Lt),
//         ("<=", Inst::Le),
//         (">", Inst::Gt),
//         (">=", Inst::Ge),
//         ("==", Inst::Eq),
//         ("!=", Inst::Ne),
//     ] {
//         let mut output = vec![];
//         VMCodeGen::new().compile(
//             &parser::Parser::new(format!("1 {} 2", op_s)).next().unwrap(),
//             &mut output,
//         );
//         assert_eq!(
//             vec![
//                 Inst::AllocLocalVar(0, 1),
//                 Inst::Push(Value::Number(1.0)),
//                 Inst::Push(Value::Number(2.0)),
//                 op_i,
//                 Inst::End,
//             ],
//             output
//         );
//     }
// }
//
// #[test]
// fn string() {
//     use parser;
//     let mut output = vec![];
//     VMCodeGen::new().compile(
//         &parser::Parser::new("\"hello\"".to_string()).next().unwrap(),
//         &mut output,
//     );
//
//     // The address (of Value::String) cannot be compared...
//     // unsafe {
//     //     assert_eq!(
//     //         vec![
//     //             Inst::AllocLocalVar(0, 1),
//     //             Inst::Push(Value::String(alloc_rawstring("hello"))), <<-- X(
//     //             Inst::End,
//     //         ],
//     //         output
//     //     );
//     // }
// }
//
// #[test]
// fn local_var_load() {
//     use parser;
//     let mut output = vec![];
//     VMCodeGen::new().compile(
//         &parser::Parser::new("var i; i".to_string()).next().unwrap(),
//         &mut output,
//     );
//     assert_eq!(
//         vec![Inst::AllocLocalVar(1, 1), Inst::GetLocal(1), Inst::End],
//         output
//     );
// }
//
// #[test]
// fn local_var_assign() {
//     use parser;
//     let mut output = vec![];
//     VMCodeGen::new().compile(
//         &parser::Parser::new("var i; i = 123".to_string())
//             .next()
//             .unwrap(),
//         &mut output,
//     );
//     assert_eq!(
//         vec![
//             Inst::AllocLocalVar(1, 1),
//             Inst::Push(Value::Number(123.0)),
//             Inst::SetLocal(1),
//             Inst::End,
//         ],
//         output
//     );
// }
//
// #[test]
// fn global_func_call() {
//     use parser;
//     for (args_s, mut args_i) in vec![
//         ("", vec![]),
//         ("1", vec![Inst::Push(Value::Number(1.0))]),
//         (
//             "1, 2",
//             vec![
//                 Inst::Push(Value::Number(1.0)),
//                 Inst::Push(Value::Number(2.0)),
//             ],
//         ),
//     ] {
//         let mut output = vec![];
//         VMCodeGen::new().compile(
//             &parser::Parser::new(format!("f({})", args_s))
//                 .next()
//                 .unwrap(),
//             &mut output,
//         );
//         let mut expect = vec![Inst::AllocLocalVar(0, 1)];
//         let args_len = args_i.len();
//         expect.append(&mut args_i);
//         expect.append(&mut vec![
//             Inst::GetGlobal("f".to_string()),
//             Inst::Call(args_len),
//             Inst::End,
//         ]);
//         assert_eq!(expect, output);
//     }
// }
//
// #[test]
// fn member_load() {
//     use parser;
//     let mut output = vec![];
//     VMCodeGen::new().compile(
//         &parser::Parser::new("console.log".to_string())
//             .next()
//             .unwrap(),
//         &mut output,
//     );
//     unsafe {
//         assert_eq!(output[0], Inst::AllocLocalVar(0, 1));
//         assert_eq!(output[1], Inst::GetGlobal("console".to_string()));
//         if let Inst::Push(Value::String(s)) = output[2] {
//             use std::ffi::CStr;
//             assert!(CStr::from_ptr(s).to_str().unwrap() == "log")
//         } else {
//             panic!()
//         }
//         assert_eq!(output[3], Inst::GetMember);
//         assert_eq!(output[4], Inst::End);
//     }
// }
//
// #[test]
// fn member_assign() {
//     let mut output = vec![];
//     // JS: a.s = 123;
//     let node = Node::StatementList(vec![Node::Assign(
//         Box::new(Node::Member(
//             Box::new(Node::Identifier("a".to_string())),
//             "s".to_string(),
//         )),
//         Box::new(Node::Number(123.0)),
//     )]);
//     VMCodeGen::new().compile(&node, &mut output);
//     unsafe {
//         assert_eq!(output[0], Inst::AllocLocalVar(0, 1));
//         assert_eq!(output[1], Inst::Push(Value::Number(123.0)));
//         assert_eq!(output[2], Inst::GetGlobal("a".to_string()));
//         if let Inst::Push(Value::String(s)) = output[3] {
//             use std::ffi::CStr;
//             assert!(CStr::from_ptr(s).to_str().unwrap() == "s")
//         } else {
//             panic!()
//         }
//         assert_eq!(output[4], Inst::SetMember);
//         assert_eq!(output[5], Inst::End);
//     }
// }
//
// #[test]
// fn while_() {
//     let mut output = vec![];
//     // JS: while(true) { }
//     let node = Node::StatementList(vec![Node::While(
//         Box::new(Node::Boolean(true)),
//         Box::new(Node::StatementList(vec![])),
//     )]);
//     VMCodeGen::new().compile(&node, &mut output);
//     assert_eq!(
//         vec![
//             Inst::AllocLocalVar(0, 1),
//             Inst::Push(Value::Bool(true)),
//             Inst::JmpIfFalse(2),
//             Inst::Jmp(-2),
//             Inst::End,
//         ],
//         output
//     );
// }
//
// #[test]
// fn if_() {
//     for (node, expect) in vec![
//         (
//             // JS: if(x < 3) ; else ;
//             Node::If(
//                 Box::new(Node::BinaryOp(
//                     Box::new(Node::Identifier("x".to_string())),
//                     Box::new(Node::Number(3.0)),
//                     BinOp::Lt,
//                 )),
//                 Box::new(Node::Nope),
//                 Box::new(Node::Nope),
//             ),
//             vec![
//                 Inst::AllocLocalVar(0, 1),
//                 Inst::GetGlobal("x".to_string()),
//                 Inst::Push(Value::Number(3.0)),
//                 Inst::Lt,
//                 Inst::JmpIfFalse(1),
//                 Inst::End,
//             ],
//         ),
//         (
//             // JS: if(x < 3) ; else x;
//             Node::If(
//                 Box::new(Node::BinaryOp(
//                     Box::new(Node::Identifier("x".to_string())),
//                     Box::new(Node::Number(3.0)),
//                     BinOp::Lt,
//                 )),
//                 Box::new(Node::Nope),
//                 Box::new(Node::Identifier("x".to_string())),
//             ),
//             vec![
//                 Inst::AllocLocalVar(0, 1),
//                 Inst::GetGlobal("x".to_string()),
//                 Inst::Push(Value::Number(3.0)),
//                 Inst::Lt,
//                 Inst::JmpIfFalse(2),
//                 Inst::Jmp(2),
//                 Inst::GetGlobal("x".to_string()),
//                 Inst::End,
//             ],
//         ),
//     ] {
//         let mut output = vec![];
//         VMCodeGen::new().compile(&node, &mut output);
//         assert_eq!(expect, output);
//     }
// }
//
// #[test]
// fn function_decl1() {
//     let mut output = vec![];
//     // JS: function f() { return 1; }
//     let node = Node::StatementList(vec![Node::FunctionDecl(
//         "f".to_string(),
//         false,
//         HashSet::new(),
//         vec![],
//         Box::new(Node::StatementList(vec![Node::Return(Some(Box::new(
//             Node::Number(1.0),
//         )))])),
//     )]);
//     VMCodeGen::new().compile(&node, &mut output);
//     assert_eq!(
//         vec![
//             Inst::AllocLocalVar(0, 1),
//             Inst::End,
//             Inst::AllocLocalVar(0, 0),
//             Inst::Push(Value::Number(1.0)),
//             Inst::Return,
//         ],
//         output
//     );
// }
//
// #[test]
// fn function_decl2() {
//     use node::FormalParameter;
//     let mut output = vec![];
//     // JS: function f(x, y) { return x + y; }
//     let node = Node::StatementList(vec![Node::FunctionDecl(
//         "f".to_string(),
//         false,
//         HashSet::new(),
//         vec![
//             FormalParameter::new("x".to_string(), None),
//             FormalParameter::new("y".to_string(), None),
//         ],
//         Box::new(Node::StatementList(vec![Node::Return(Some(Box::new(
//             Node::BinaryOp(
//                 Box::new(Node::Identifier("x".to_string())),
//                 Box::new(Node::Identifier("y".to_string())),
//                 BinOp::Add,
//             ),
//         )))])),
//     )]);
//     VMCodeGen::new().compile(&node, &mut output);
//     assert_eq!(
//         vec![
//             Inst::AllocLocalVar(0, 1),
//             Inst::End,
//             Inst::AllocLocalVar(0, 2),
//             Inst::GetLocal(0),
//             Inst::GetLocal(1),
//             Inst::Add,
//             Inst::Return,
//         ],
//         output
//     );
// }
//
// #[test]
// fn new() {
//     let mut output = vec![];
//     // JS: new (function(){this.x = 123})
//     let node = Node::StatementList(vec![
//         Node::New(Box::new(Node::Call(
//             Box::new(Node::Identifier("anonymous.3035120513".to_string())),
//             vec![],
//         ))),
//         Node::FunctionDecl(
//             "anonymous.3035120513".to_string(),
//             true,
//             HashSet::new(),
//             vec![],
//             Box::new(Node::StatementList(vec![Node::Assign(
//                 Box::new(Node::Member(Box::new(Node::This), "x".to_string())),
//                 Box::new(Node::Number(123.0)),
//             )])),
//         ),
//     ]);
//     VMCodeGen::new().compile(&node, &mut output);
//     assert_eq!(
//         vec![
//             Inst::AllocLocalVar(0, 1),
//             Inst::Push(Value::NeedThis(Box::new(Value::Function(
//                 4,
//                 Rc::new(RefCell::new(HashMap::new())),
//             )))),
//             Inst::Constract(0),
//             Inst::End,
//             Inst::AllocLocalVar(0, 1),
//             Inst::Push(Value::Number(123.0)),
//             Inst::PushThis,
//         ],
//         output[0..7].to_vec(),
//     );
//     if let Inst::Push(Value::String(s)) = output[7] {
//         unsafe {
//             use std::ffi::CStr;
//             assert!(CStr::from_ptr(s).to_str().unwrap() == "x")
//         }
//     } else {
//         panic!()
//     }
//     assert_eq!(
//         vec![Inst::SetMember, Inst::Push(Value::Undefined), Inst::Return],
//         output[8..11].to_vec()
//     );
// }
