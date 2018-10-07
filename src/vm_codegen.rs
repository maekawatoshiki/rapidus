use bytecode_gen::{ByteCode, ByteCodeGen, VMInst};
use node::{BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp};
use vm::{new_value_function, CallObject, CallObjectRef, Value};

use std::collections::HashMap;
use std::ffi::CString;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub params: FormalParameters,
    pub insts: ByteCode,
}

impl FunctionInfo {
    pub fn new(name: String, params: FormalParameters, insts: ByteCode) -> FunctionInfo {
        FunctionInfo {
            name: name,
            params: params,
            insts: insts,
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
        insts: &mut ByteCode,
        break_label_pos: isize,
    ) {
        for jmp_pos in &self.break_jmp_list {
            bytecode_gen.replace_int32(
                (break_label_pos - jmp_pos) as i32 - 5,
                &mut insts[*jmp_pos as usize + 1..*jmp_pos as usize + 5],
            );
        }
        self.break_jmp_list.clear();
    }

    fn replace_continue_jmps(
        &mut self,
        bytecode_gen: &mut ByteCodeGen,
        insts: &mut ByteCode,
        continue_label_pos: isize,
    ) {
        for jmp_pos in &self.continue_jmp_list {
            bytecode_gen.replace_int32(
                (continue_label_pos - jmp_pos) as i32 - 5,
                &mut insts[*jmp_pos as usize + 1..*jmp_pos as usize + 5],
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
    pub fn compile(&mut self, node: &Node, insts: &mut ByteCode) {
        let pos = insts.len();
        self.bytecode_gen.gen_create_context(0, insts);

        self.run(node, insts);

        self.bytecode_gen
            .replace_int32(0, &mut insts[pos + 1..pos + 5]);

        self.bytecode_gen.gen_end(insts);

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
                insts: func_insts,
            },
        ) in &self.functions
        {
            let pos = insts.len();
            let val = new_value_function(pos, {
                let mut callobj =
                    CallObject::new(Value::Object(self.global_varmap.borrow().vals.clone()));
                // TODO: Implement rest paramter
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
            self.global_varmap
                .borrow_mut()
                .set_value(name.clone(), val.clone());
            insts.append(&mut func_insts.clone());
        }

        // let mut i = 0;
        // while i < insts.len() {
        //     let inst_size = VMInst::get_inst_size(insts[i])
        //         .unwrap_or_else(|| panic!("Illegal VM Instruction occurred"));
        //     match insts[i] {
        //         VMInst::GET_NAME => {
        //             let id = insts[i + 1] as i32
        //                 + ((insts[i + 2] as i32) << 8)
        //                 + ((insts[i + 3] as i32) << 16)
        //                 + ((insts[i + 4] as i32) << 24);
        //             if let Some(val) = function_value_list
        //                 .get(self.bytecode_gen.const_table.string[id as usize].as_str())
        //             {
        //                 match val {
        //                     Value::NeedThis(callee) => {
        //                         insts[i] = VMInst::PUSH_CONST;
        //                         let id = self.bytecode_gen.const_table.value.len();
        //                         self.bytecode_gen
        //                             .const_table
        //                             .value
        //                             .push(Value::NeedThis(callee.clone()));
        //                         self.bytecode_gen
        //                             .replace_int32(id as i32, &mut insts[i + 1..i + 5]);
        //                     }
        //                     _ => {
        //                         insts[i] = VMInst::PUSH_CONST;
        //                         let id = self.bytecode_gen.const_table.value.len();
        //                         self.bytecode_gen.const_table.value.push(val.clone());
        //                         self.bytecode_gen
        //                             .replace_int32(id as i32, &mut insts[i + 1..i + 5]);
        //                     }
        //                 }
        //             }
        //             i += inst_size
        //         }
        //         _ => i += inst_size,
        //     }
        // }
    }

    fn run(&mut self, node: &Node, insts: &mut ByteCode) {
        match &node.base {
            &NodeBase::StatementList(ref node_list) => self.run_statement_list(node_list, insts),
            &NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.run_function_decl(name, params, &*body)
            }
            &NodeBase::VarDecl(ref name, ref init) => {
                self.run_var_decl(name, init, insts);
            }
            &NodeBase::If(ref cond, ref then_, ref else_) => {
                self.run_if(&*cond, &*then_, &*else_, insts)
            }
            &NodeBase::While(ref cond, ref body) => self.run_while(&*cond, &*body, insts),
            &NodeBase::For(ref init, ref cond, ref step, ref body) => {
                self.run_for(&*init, &*cond, &*step, &*body, insts)
            }
            &NodeBase::Assign(ref dst, ref src) => self.run_assign(&*dst, &*src, insts),
            &NodeBase::UnaryOp(ref expr, ref op) => self.run_unary_op(&*expr, op, insts),
            &NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.run_binary_op(&*lhs, &*rhs, op, insts)
            }
            &NodeBase::Call(ref callee, ref args) => self.run_call(&*callee, args, insts),
            &NodeBase::Member(ref parent, ref member) => self.run_member(&*parent, member, insts),
            &NodeBase::Index(ref parent, ref idx) => self.run_index(&*parent, &*idx, insts),
            &NodeBase::Return(ref val) => self.run_return(val, insts),
            &NodeBase::Break => self.run_break(insts),
            &NodeBase::Continue => self.run_continue(insts),
            &NodeBase::New(ref expr) => self.run_new_expr(&*expr, insts),
            &NodeBase::Object(ref properties) => self.run_object_literal(properties, insts),
            &NodeBase::Array(ref properties) => self.run_array_literal(properties, insts),
            &NodeBase::Identifier(ref name) => self.run_identifier(name, insts),
            &NodeBase::This => self.bytecode_gen.gen_push_this(insts),
            &NodeBase::Arguments => self.bytecode_gen.gen_push_arguments(insts),
            &NodeBase::String(ref s) => self
                .bytecode_gen
                .gen_push_const(Value::String(CString::new(s.as_str()).unwrap()), insts),
            // When 'n' is an integer
            &NodeBase::Number(n) if n - n.floor() == 0.0 => {
                if -128.0 < n && n < 127.0 {
                    self.bytecode_gen.gen_push_int8(n as i8, insts)
                } else {
                    self.bytecode_gen.gen_push_int32(n as i32, insts)
                }
            }
            &NodeBase::Number(n) => self.bytecode_gen.gen_push_const(Value::Number(n), insts),
            &NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, insts),
            &NodeBase::SetCurCallObj(ref name) => {
                self.bytecode_gen.gen_get_name(name, insts);
                self.bytecode_gen.gen_set_cur_callobj(insts);
            }
            _ => {}
        }
    }
}

impl VMCodeGen {
    pub fn run_statement_list(&mut self, node_list: &Vec<Node>, insts: &mut ByteCode) {
        for node in node_list {
            self.run(node, insts)
        }
    }
}

impl VMCodeGen {
    pub fn run_function_decl(&mut self, name: &String, params: &FormalParameters, body: &Node) {
        let mut func_insts = vec![];

        self.bytecode_gen.gen_create_context(0, &mut func_insts);

        // TODO: Implement Rest Parameter ASAP

        self.run(body, &mut func_insts);

        match func_insts.last() {
            Some(&VMInst::RETURN) => {}
            _ => {
                self.bytecode_gen
                    .gen_push_const(Value::Undefined, &mut func_insts);
                self.bytecode_gen.gen_return(&mut func_insts);
            }
        }

        self.bytecode_gen.replace_int32(0, &mut func_insts[1..5]);

        self.functions.insert(
            name.clone(),
            FunctionInfo::new(name.clone(), params.clone(), func_insts),
        );
    }

    pub fn run_return(&mut self, val: &Option<Box<Node>>, insts: &mut ByteCode) {
        if let &Some(ref val) = val {
            self.run(&*val, insts)
        } else {
            self.bytecode_gen.gen_push_const(Value::Undefined, insts);
        }
        self.bytecode_gen.gen_return(insts);
    }
}

impl VMCodeGen {
    pub fn run_break(&mut self, insts: &mut ByteCode) {
        let break_jmp_pos = insts.len() as isize;
        self.bytecode_gen.gen_jmp(0, insts);
        self.labels
            .last_mut()
            .unwrap()
            .break_jmp_list
            .push(break_jmp_pos);
    }

    pub fn run_continue(&mut self, insts: &mut ByteCode) {
        let continue_jmp_pos = insts.len() as isize;
        self.bytecode_gen.gen_jmp(0, insts);
        self.labels
            .last_mut()
            .unwrap()
            .continue_jmp_list
            .push(continue_jmp_pos);
    }
}

impl VMCodeGen {
    pub fn run_new_expr(&mut self, expr: &Node, insts: &mut ByteCode) {
        self.run(expr, insts);
        let len = insts.len();
        if insts[len - 1 - 4] == VMInst::CALL {
            insts[len - 1 - 4] = VMInst::CONSTRUCT;
        } else {
            unreachable!()
        }
    }
}

impl VMCodeGen {
    pub fn run_var_decl(&mut self, name: &String, init: &Option<Box<Node>>, insts: &mut ByteCode) {
        if let &Some(ref init) = init {
            self.run(&*init, insts);
        } else {
            self.bytecode_gen.gen_push_const(Value::Undefined, insts);
        }
        self.bytecode_gen.gen_decl_var(name, insts);
    }
}

impl VMCodeGen {
    pub fn run_if(&mut self, cond: &Node, then_: &Node, else_: &Node, insts: &mut ByteCode) {
        self.run(cond, insts);

        let cond_pos = insts.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, insts);

        self.run(then_, insts);

        if else_.base == NodeBase::Nope {
            let pos = insts.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut insts[cond_pos as usize + 1..cond_pos as usize + 5],
            );
        } else {
            let then_end_pos = insts.len() as isize;
            self.bytecode_gen.gen_jmp(0, insts);

            let pos = insts.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut insts[cond_pos as usize + 1..cond_pos as usize + 5],
            );

            self.run(else_, insts);

            let pos = insts.len() as isize;
            self.bytecode_gen.replace_int32(
                (pos - then_end_pos) as i32 - 5,
                &mut insts[then_end_pos as usize + 1..then_end_pos as usize + 5],
            );
        }
    }

    pub fn run_while(&mut self, cond: &Node, body: &Node, insts: &mut ByteCode) {
        let pos1 = insts.len() as isize;
        self.labels.push(Labels::new());

        self.run(cond, insts);

        let cond_pos = insts.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, insts);

        self.run(body, insts);

        let loop_pos = insts.len() as isize;
        self.bytecode_gen
            .gen_jmp((pos1 - loop_pos) as i32 - 5, insts);

        let break_label_pos = insts.len() as isize;
        self.labels.last_mut().unwrap().replace_break_jmps(
            &mut self.bytecode_gen,
            insts,
            break_label_pos,
        );
        self.labels
            .last_mut()
            .unwrap()
            .replace_continue_jmps(&mut self.bytecode_gen, insts, pos1);
        self.labels.pop();

        let pos2 = insts.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos2 - cond_pos) as i32 - 5,
            &mut insts[cond_pos as usize + 1..cond_pos as usize + 5],
        );
    }

    pub fn run_for(
        &mut self,
        init: &Node,
        cond: &Node,
        step: &Node,
        body: &Node,
        insts: &mut ByteCode,
    ) {
        self.run(init, insts);

        let pos = insts.len() as isize;
        self.labels.push(Labels::new());

        self.run(cond, insts);

        let cond_pos = insts.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, insts);

        self.run(body, insts);

        let continue_label_pos = insts.len() as isize;
        self.labels.last_mut().unwrap().replace_continue_jmps(
            &mut self.bytecode_gen,
            insts,
            continue_label_pos,
        );
        self.run(step, insts);

        let loop_pos = insts.len() as isize;
        self.bytecode_gen
            .gen_jmp((pos - loop_pos) as i32 - 5, insts);

        let break_label_pos = insts.len() as isize;
        self.labels.last_mut().unwrap().replace_break_jmps(
            &mut self.bytecode_gen,
            insts,
            break_label_pos,
        );
        self.labels.pop();

        let pos = insts.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut insts[cond_pos as usize + 1..cond_pos as usize + 5],
        );
    }
}

impl VMCodeGen {
    pub fn run_unary_op(&mut self, expr: &Node, op: &UnaryOp, insts: &mut ByteCode) {
        self.run(expr, insts);
        match op {
            &UnaryOp::Minus => self.bytecode_gen.gen_neg(insts),
            _ => unimplemented!(),
        }
    }

    pub fn run_binary_op(&mut self, lhs: &Node, rhs: &Node, op: &BinOp, insts: &mut ByteCode) {
        // Editing this (LAnd and LOr) source code has influence on the source code of JIT
        // compiler(src/jit.rs).
        match op {
            &BinOp::LAnd => {
                self.run(lhs, insts);

                self.bytecode_gen.gen_double(insts);

                let lhs_cond_pos = insts.len() as isize;
                self.bytecode_gen.gen_jmp_if_false(0, insts);

                self.bytecode_gen.gen_pop(insts);

                self.run(rhs, insts);

                let pos = insts.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut insts[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                self.bytecode_gen.gen_land(insts);
                return;
            }
            &BinOp::LOr => {
                self.run(lhs, insts);

                self.bytecode_gen.gen_double(insts);

                let lhs_cond_pos = insts.len() as isize;
                self.bytecode_gen.gen_jmp_if_false(0, insts);

                let lhs_true_pos = insts.len() as isize;
                self.bytecode_gen.gen_jmp(0, insts);

                let pos = insts.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut insts[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                self.bytecode_gen.gen_pop(insts);

                self.run(rhs, insts);

                let pos = insts.len() as isize;
                self.bytecode_gen.replace_int32(
                    (pos - lhs_true_pos) as i32 - 5,
                    &mut insts[lhs_true_pos as usize + 1..lhs_true_pos as usize + 5],
                );

                self.bytecode_gen.gen_lor(insts);
                return;
            }
            _ => {}
        };

        self.run(lhs, insts);
        self.run(rhs, insts);
        match op {
            &BinOp::Add => self.bytecode_gen.gen_add(insts),
            &BinOp::Sub => self.bytecode_gen.gen_sub(insts),
            &BinOp::Mul => self.bytecode_gen.gen_mul(insts),
            &BinOp::Div => self.bytecode_gen.gen_div(insts),
            &BinOp::Rem => self.bytecode_gen.gen_rem(insts),
            &BinOp::Eq => self.bytecode_gen.gen_eq(insts),
            &BinOp::Ne => self.bytecode_gen.gen_ne(insts),
            &BinOp::SEq => self.bytecode_gen.gen_seq(insts),
            &BinOp::SNe => self.bytecode_gen.gen_sne(insts),
            &BinOp::And => self.bytecode_gen.gen_and(insts),
            &BinOp::Or => self.bytecode_gen.gen_or(insts),
            &BinOp::Lt => self.bytecode_gen.gen_lt(insts),
            &BinOp::Gt => self.bytecode_gen.gen_gt(insts),
            &BinOp::Le => self.bytecode_gen.gen_le(insts),
            &BinOp::Ge => self.bytecode_gen.gen_ge(insts),
            _ => {}
        }
    }

    pub fn run_assign(&mut self, dst: &Node, src: &Node, insts: &mut ByteCode) {
        self.run(src, insts);

        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.bytecode_gen.gen_set_name(name, insts);
            }
            NodeBase::Member(ref parent, ref member) => {
                self.run(&*parent, insts);
                self.bytecode_gen
                    .gen_push_const(Value::String(CString::new(member.as_str()).unwrap()), insts);
                self.bytecode_gen.gen_set_member(insts);
            }
            NodeBase::Index(ref parent, ref idx) => {
                self.run(&*parent, insts);
                self.run(&*idx, insts);
                self.bytecode_gen.gen_set_member(insts);
            }
            _ => unimplemented!(),
        }
    }
}

impl VMCodeGen {
    pub fn run_call(&mut self, callee: &Node, args: &Vec<Node>, insts: &mut ByteCode) {
        for arg in args {
            self.run(arg, insts);
        }

        self.run(callee, insts);

        self.bytecode_gen.gen_call(args.len() as u32, insts);
    }
}

impl VMCodeGen {
    fn run_object_literal(&mut self, properties: &Vec<PropertyDefinition>, insts: &mut ByteCode) {
        for property in properties {
            match property {
                PropertyDefinition::IdentifierReference(_) => unimplemented!(),
                PropertyDefinition::Property(name, node) => {
                    self.run(&node, insts);
                    self.bytecode_gen
                        .gen_push_const(Value::String(CString::new(name.as_str()).unwrap()), insts);
                }
            }
        }

        self.bytecode_gen
            .gen_create_object(properties.len() as usize, insts);
    }

    fn run_array_literal(&mut self, elems: &Vec<Node>, insts: &mut ByteCode) {
        for elem in elems.iter().rev() {
            self.run(elem, insts);
        }

        self.bytecode_gen
            .gen_create_array(elems.len() as usize, insts);
    }
}

impl VMCodeGen {
    fn run_member(&mut self, parent: &Node, member: &String, insts: &mut ByteCode) {
        self.run(parent, insts);

        self.bytecode_gen
            .gen_push_const(Value::String(CString::new(member.as_str()).unwrap()), insts);
        self.bytecode_gen.gen_get_member(insts);
    }

    fn run_index(&mut self, parent: &Node, idx: &Node, insts: &mut ByteCode) {
        self.run(parent, insts);

        self.run(idx, insts);
        self.bytecode_gen.gen_get_member(insts);
    }

    fn run_identifier(&mut self, name: &String, insts: &mut ByteCode) {
        self.bytecode_gen.gen_get_name(name, insts);
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
