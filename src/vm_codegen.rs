use builtin;
use bytecode_gen::{ByteCode, ByteCodeGen};
use id::{Id, IdGen};
use node::{
    BinOp, FormalParameters, FunctionDeclNode, Node, NodeBase, PropertyDefinition, UnaryOp,
};
use std::collections::HashSet;
use vm::Value;
use vm::{
    new_value_function, PUSH_INT32, PUSH_INT8, ADD, AND, ASG_FREST_PARAM, CALL, CONSTRUCT,
    CREATE_ARRAY, CREATE_CONTEXT, CREATE_OBJECT, DIV, DOUBLE, END, EQ, GE, GET_ARG_LOCAL,
    GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP, JMP_IF_FALSE, LAND, LE, LOR, LT, MUL, NE, NEG, OR,
    POP, PUSH_ARGUMENTS, PUSH_CONST, PUSH_FALSE, PUSH_THIS, PUSH_TRUE, REM, RETURN, SEQ,
    SET_ARG_LOCAL, SET_GLOBAL, SET_LOCAL, SET_MEMBER, SNE, SUB,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub use_this: bool,
    pub insts: ByteCode,
}

impl FunctionInfo {
    pub fn new(name: String, use_this: bool, insts: ByteCode) -> FunctionInfo {
        FunctionInfo {
            name: name,
            use_this: use_this,
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
    pub global_varmap: HashMap<String, Value>, // usize will be replaced with an appropriate type
    pub local_varmap: Vec<HashMap<String, (bool, usize)>>, // hashmap<name, (is_arg_var, id)>
    pub functions: HashMap<String, FunctionInfo>,
    pub local_var_stack_addr: IdGen,
    pub arguemnt_var_addr: IdGen,
    pub bytecode_gen: ByteCodeGen,
    pub labels: Vec<Labels>,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: HashMap::new(),
            local_varmap: vec![HashMap::new()],
            functions: HashMap::new(),
            local_var_stack_addr: IdGen::new(),
            arguemnt_var_addr: IdGen::new(),
            bytecode_gen: ByteCodeGen::new(),
            labels: vec![Labels::new()],
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, insts: &mut ByteCode) {
        let pos = insts.len();
        self.bytecode_gen.gen_create_context(0, insts);

        self.run_arg_var_decl(&"this".to_string(), &None, insts);

        self.run(node, insts);

        self.bytecode_gen.replace_int32(
            self.local_var_stack_addr.get_cur_id() as i32,
            &mut insts[pos + 1..pos + 5],
        );

        self.bytecode_gen.gen_end(insts);

        let mut function_value_list = HashMap::new();

        {
            function_value_list.insert("console".to_string(), {
                let mut map = HashMap::new();
                map.insert(
                    "log".to_string(),
                    Value::BuiltinFunction(builtin::CONSOLE_LOG),
                );
                Value::Object(Rc::new(RefCell::new(map)))
            });

            function_value_list.insert("process".to_string(), {
                let mut map = HashMap::new();
                map.insert("stdout".to_string(), {
                    let mut map = HashMap::new();
                    map.insert(
                        "write".to_string(),
                        Value::BuiltinFunction(builtin::PROCESS_STDOUT_WRITE),
                    );
                    Value::Object(Rc::new(RefCell::new(map)))
                });
                Value::Object(Rc::new(RefCell::new(map)))
            });

            function_value_list.insert("Math".to_string(), {
                let mut map = HashMap::new();
                map.insert(
                    "floor".to_string(),
                    Value::BuiltinFunction(builtin::MATH_FLOOR),
                );
                map.insert(
                    "random".to_string(),
                    Value::BuiltinFunction(builtin::MATH_RANDOM),
                );
                map.insert("pow".to_string(), Value::BuiltinFunction(builtin::MATH_POW));
                Value::Object(Rc::new(RefCell::new(map)))
            });
        }

        for (
            _,
            FunctionInfo {
                name,
                use_this,
                insts: func_insts,
            },
        ) in &self.functions
        {
            let pos = insts.len();
            let mut val;
            if *use_this {
                val = Value::NeedThis(Box::new(new_value_function(pos)));
                self.global_varmap.insert(name.clone(), val.clone());
            } else {
                val = new_value_function(pos);
                self.global_varmap.insert(name.clone(), val.clone());
            }
            function_value_list.insert(name.clone(), val.clone());

            let mut func_insts = func_insts.clone();
            insts.append(&mut func_insts);
        }

        let mut i = 0;
        while i < insts.len() {
            match insts[i] {
                ASG_FREST_PARAM => i += 9,
                CREATE_CONTEXT => i += 5,
                CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | SET_GLOBAL | GET_LOCAL
                | SET_ARG_LOCAL | GET_ARG_LOCAL | CREATE_ARRAY | SET_LOCAL | JMP_IF_FALSE | JMP
                | CALL => i += 5,
                PUSH_INT8 => i += 2,
                PUSH_FALSE | END | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT
                | PUSH_ARGUMENTS | NEG | GT | LE | GE | EQ | NE | GET_MEMBER | RETURN | SNE
                | LAND | POP | DOUBLE | AND | OR | SEQ | SET_MEMBER | LOR => i += 1,
                GET_GLOBAL => {
                    let id = insts[i + 1] as i32
                        + ((insts[i + 2] as i32) << 8)
                        + ((insts[i + 3] as i32) << 16)
                        + ((insts[i + 4] as i32) << 24);
                    if let Some(val) = function_value_list
                        .get(self.bytecode_gen.const_table.string[id as usize].as_str())
                    {
                        match val {
                            Value::NeedThis(callee) => {
                                insts[i] = PUSH_CONST;
                                let id = self.bytecode_gen.const_table.value.len();
                                self.bytecode_gen
                                    .const_table
                                    .value
                                    .push(Value::NeedThis(callee.clone()));
                                self.bytecode_gen
                                    .replace_int32(id as i32, &mut insts[i + 1..i + 5]);
                            }
                            _ => {
                                insts[i] = PUSH_CONST;
                                let id = self.bytecode_gen.const_table.value.len();
                                self.bytecode_gen.const_table.value.push(val.clone());
                                self.bytecode_gen
                                    .replace_int32(id as i32, &mut insts[i + 1..i + 5]);
                            }
                        }
                    }
                    i += 5;
                }
                _ => unreachable!(),
            }
        }
    }

    fn run(&mut self, node: &Node, insts: &mut ByteCode) {
        match &node.base {
            &NodeBase::StatementList(ref node_list) => self.run_statement_list(node_list, insts),
            &NodeBase::FunctionDecl(FunctionDeclNode {
                ref name,
                ref mangled_name,
                ref use_this,
                ref fv,
                ref params,
                ref body,
            }) => self.run_function_decl(
                if let Some(ref mangled_name) = mangled_name {
                    mangled_name
                } else {
                    name
                },
                *use_this,
                fv,
                params,
                &*body,
            ),
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
            &NodeBase::Number(n) if n - n.floor() == 0.0 => {
                // When 'n' is an integer
                if -128.0 < n && n < 127.0 {
                    self.bytecode_gen.gen_push_int8(n as i8, insts)
                } else {
                    self.bytecode_gen.gen_push_int32(n as i32, insts)
                }
            }
            &NodeBase::Number(n) => self.bytecode_gen.gen_push_const(Value::Number(n), insts),
            &NodeBase::Boolean(b) => self.bytecode_gen.gen_push_bool(b, insts),
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
    pub fn run_function_decl(
        &mut self,
        name: &String,
        use_this: bool,
        fv: &HashSet<String>,
        params: &FormalParameters,
        body: &Node,
    ) {
        assert_eq!(fv.len(), 0);

        let name = name.clone();

        self.local_varmap.push(HashMap::new());
        self.local_var_stack_addr.save();
        self.arguemnt_var_addr.save();

        let mut func_insts = vec![];

        self.bytecode_gen.gen_create_context(0, &mut func_insts);

        if use_this {
            self.run_arg_var_decl(&"this".to_string(), &None, &mut func_insts);
        }

        for param in params {
            if param.is_rest_param {
                let id = self.run_var_decl(&param.name, &None, &mut func_insts);
                self.bytecode_gen.gen_assign_func_rest_param(
                    if use_this { 1 } else { 0 } + params.len() - /*rest param itself->*/ 1,
                    id,
                    &mut func_insts,
                );
            } else {
                self.run_arg_var_decl(&param.name, &param.init, &mut func_insts);
            }
        }

        self.run(body, &mut func_insts);

        match func_insts.last() {
            Some(&RETURN) => {}
            _ => {
                self.bytecode_gen
                    .gen_push_const(Value::Undefined, &mut func_insts);
                self.bytecode_gen.gen_return(&mut func_insts);
            }
        }

        self.bytecode_gen.replace_int32(
            self.local_var_stack_addr.get_cur_id() as i32,
            &mut func_insts[1..5],
        );

        self.local_var_stack_addr.restore();
        self.arguemnt_var_addr.restore();
        self.local_varmap.pop();

        self.functions.insert(
            name.clone(),
            FunctionInfo::new(name.clone(), use_this, func_insts),
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
        if insts[len - 1 - 4] == CALL {
            insts[len - 1 - 4] = CONSTRUCT;
        } else {
            unreachable!()
        }
    }
}

impl VMCodeGen {
    pub fn run_var_decl(
        &mut self,
        name: &String,
        init: &Option<Box<Node>>,
        insts: &mut ByteCode,
    ) -> Id {
        if let Some(id) = self.local_varmap.last().unwrap().get(name) {
            return id.1;
        }

        let id = self.local_var_stack_addr.gen_id();

        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), (false, id));

        if let &Some(ref init) = init {
            self.run(&*init, insts);
            self.bytecode_gen.gen_set_local(id as u32, insts);
        }

        id
    }

    pub fn run_arg_var_decl(&mut self, name: &String, init: &Option<Node>, insts: &mut ByteCode) {
        let id = self.arguemnt_var_addr.gen_id();

        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), (true, id));

        if let &Some(ref init) = init {
            self.run(init, insts);
            self.bytecode_gen.gen_set_local(id as u32, insts);
        }
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
                if let Some((is_arg, p)) = self.local_varmap.last().unwrap().get(name.as_str()) {
                    if *is_arg {
                        self.bytecode_gen.gen_set_arg_local(*p as u32, insts);
                    } else {
                        self.bytecode_gen.gen_set_local(*p as u32, insts);
                    }
                } else {
                    self.bytecode_gen.gen_set_global(name.clone(), insts);
                }
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
        if let Some((is_arg, p)) = self.local_varmap.last().unwrap().get(name.as_str()) {
            if *is_arg {
                self.bytecode_gen.gen_get_arg_local(*p as u32, insts);
            } else {
                self.bytecode_gen.gen_get_local(*p as u32, insts);
            }
        } else {
            self.bytecode_gen.gen_get_global(name.clone(), insts);
        }
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
