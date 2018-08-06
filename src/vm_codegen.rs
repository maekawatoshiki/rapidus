use bytecode_gen::{ByteCode, ByteCodeGen};
use id::IdGen;
use node::{BinOp, FormalParameters, Node, PropertyDefinition};
use std::collections::HashSet;
use vm::Value;
use vm::{
    PUSH_INT32, PUSH_INT8, ADD, CALL, CONSTRACT, CREATE_CONTEXT, CREATE_OBJECT, DIV, END, EQ, GE,
    GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP, JMP_IF_FALSE, LE, LT, MUL, NE, PUSH_CONST,
    PUSH_FALSE, PUSH_THIS, PUSH_TRUE, REM, RETURN, SET_GLOBAL, SET_LOCAL, SET_MEMBER, SUB,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub use_this: bool,
    pub insts: ByteCode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PendingFunctionInfo {
    pub name: String,
    pub use_this: bool,
    pub fv_name: Vec<String>,
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

impl PendingFunctionInfo {
    pub fn new(
        name: String,
        use_this: bool,
        fv_name: Vec<String>,
        insts: ByteCode,
    ) -> PendingFunctionInfo {
        PendingFunctionInfo {
            name: name,
            use_this: use_this,
            insts: insts,
            fv_name: fv_name,
        }
    }
}

#[derive(Clone, Debug)]
pub struct VMCodeGen {
    pub global_varmap: HashMap<String, Value>, // usize will be replaced with an appropriate type
    pub local_varmap: Vec<HashMap<String, usize>>,
    pub functions: HashMap<String, FunctionInfo>,
    pub local_var_stack_addr: IdGen,
    pub bytecode_gen: ByteCodeGen,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: HashMap::new(),
            local_varmap: vec![HashMap::new()],
            functions: HashMap::new(),
            local_var_stack_addr: IdGen::new(),
            bytecode_gen: ByteCodeGen::new(),
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, insts: &mut ByteCode) {
        let pos = insts.len();
        self.bytecode_gen.gen_create_context(0, 0, insts);
        // insts.push(Inst::AllocLocalVar(0, 0));

        self.run_var_decl2(&"this".to_string(), &None, insts);

        self.run(node, insts);

        self.bytecode_gen.replace_int32(
            self.local_var_stack_addr.get_cur_id() as i32 - 1, /* for this */
            &mut insts[pos + 1..pos + 5],
        );
        self.bytecode_gen
            .replace_int32(1 /* for this */, &mut insts[pos + 5..pos + 9]);

        self.bytecode_gen.gen_end(insts);

        let mut function_value_list = HashMap::new();

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
                val = Value::NeedThis(Box::new(Value::Function(
                    pos,
                    Rc::new(RefCell::new(HashMap::new())),
                )));
                self.global_varmap.insert(name.clone(), val.clone());
            } else {
                val = Value::Function(pos, Rc::new(RefCell::new(HashMap::new())));
                self.global_varmap.insert(name.clone(), val.clone());
            }
            function_value_list.insert(name.clone(), val.clone());

            let mut func_insts = func_insts.clone();
            insts.append(&mut func_insts);
        }

        let mut i = 0;
        while i < insts.len() {
            match insts[i] {
                END => i += 1,
                CREATE_CONTEXT => i += 9,
                CONSTRACT => i += 5,
                CREATE_OBJECT => i += 5,
                PUSH_INT8 => i += 2,
                PUSH_INT32 => i += 5,
                PUSH_FALSE => i += 1,
                PUSH_TRUE => i += 1,
                PUSH_CONST => i += 5,
                PUSH_THIS => i += 1,
                ADD => i += 1,
                SUB => i += 1,
                MUL => i += 1,
                DIV => i += 1,
                REM => i += 1,
                LT => i += 1,
                GT => i += 1,
                LE => i += 1,
                GE => i += 1,
                EQ => i += 1,
                NE => i += 1,
                GET_MEMBER => i += 1,
                SET_MEMBER => i += 1,
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
                SET_GLOBAL => i += 5,
                GET_LOCAL => i += 5,
                SET_LOCAL => i += 5,
                JMP_IF_FALSE => i += 5,
                JMP => i += 5,
                CALL => i += 5,
                RETURN => i += 1,
                _ => unreachable!(),
            }
        }
    }

    fn run(&mut self, node: &Node, insts: &mut ByteCode) {
        match node {
            &Node::StatementList(ref node_list) => self.run_statement_list(node_list, insts),
            &Node::FunctionDecl(ref name, ref use_this, ref fv, ref params, ref body) => {
                self.run_function_decl(name, *use_this, fv, params, &*body)
            }
            &Node::VarDecl(ref name, ref init) => self.run_var_decl(name, init, insts),
            &Node::If(ref cond, ref then_, ref else_) => {
                self.run_if(&*cond, &*then_, &*else_, insts)
            }
            &Node::While(ref cond, ref body) => self.run_while(&*cond, &*body, insts),
            &Node::Assign(ref dst, ref src) => self.run_assign(&*dst, &*src, insts),
            &Node::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.run_binary_op(&*lhs, &*rhs, op, insts)
            }
            &Node::Call(ref callee, ref args) => self.run_call(&*callee, args, insts),
            &Node::Member(ref parent, ref member) => self.run_member(&*parent, member, insts),
            &Node::Index(ref parent, ref idx) => self.run_index(&*parent, &*idx, insts),
            &Node::Return(ref val) => self.run_return(val, insts),
            &Node::New(ref expr) => self.run_new_expr(&*expr, insts),
            &Node::Object(ref properties) => self.run_object_literal(properties, insts),
            &Node::Identifier(ref name) => self.run_identifier(name, insts),
            &Node::This => self.bytecode_gen.gen_push_this(insts),
            &Node::String(ref s) => self
                .bytecode_gen
                .gen_push_const(Value::String(s.clone()), insts),
            &Node::Number(n) if n - n.floor() == 0.0 => {
                // When 'n' is an integer
                if -128.0 < n && n < 127.0 {
                    self.bytecode_gen.gen_push_int8(n as i8, insts)
                } else {
                    self.bytecode_gen.gen_push_int32(n as i32, insts)
                }
            }
            &Node::Number(n) => self.bytecode_gen.gen_push_const(Value::Number(n), insts),
            &Node::Boolean(b) => self.bytecode_gen.gen_push_bool(b, insts),
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

        let mut func_insts = vec![];

        self.bytecode_gen.gen_create_context(0, 0, &mut func_insts);

        if use_this {
            self.run_var_decl2(&"this".to_string(), &None, &mut func_insts);
        }

        for param in params {
            self.run_var_decl2(&param.name, &param.init, &mut func_insts)
        }

        let params_len = params.len() + if use_this { 1 } else { 0 };

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
            (self.local_var_stack_addr.get_cur_id() - params_len) as i32,
            &mut func_insts[1..5],
        );
        self.bytecode_gen
            .replace_int32(params_len as i32, &mut func_insts[5..9]);

        self.local_var_stack_addr.restore();
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
    pub fn run_new_expr(&mut self, expr: &Node, insts: &mut ByteCode) {
        self.run(expr, insts);
        let len = insts.len();
        if insts[len - 1 - 4] == CALL {
            insts[len - 1 - 4] = CONSTRACT;
        } else {
            unreachable!()
        }
    }
}

impl VMCodeGen {
    pub fn run_var_decl(&mut self, name: &String, init: &Option<Box<Node>>, insts: &mut ByteCode) {
        let id = self.local_var_stack_addr.gen_id();

        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), id);

        if let &Some(ref init) = init {
            self.run(&*init, insts);
            self.bytecode_gen.gen_set_local(id as u32, insts);
        }
    }

    pub fn run_var_decl2(&mut self, name: &String, init: &Option<Node>, insts: &mut ByteCode) {
        let id = self.local_var_stack_addr.gen_id();

        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), id);

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

        if else_ == &Node::Nope {
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
        let pos = insts.len() as isize;

        self.run(cond, insts);

        let cond_pos = insts.len() as isize;
        self.bytecode_gen.gen_jmp_if_false(0, insts);

        self.run(body, insts);

        let loop_pos = insts.len() as isize;
        self.bytecode_gen
            .gen_jmp((pos - loop_pos) as i32 - 5, insts);

        let pos = insts.len() as isize;
        self.bytecode_gen.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut insts[cond_pos as usize + 1..cond_pos as usize + 5],
        );
    }
}

impl VMCodeGen {
    pub fn run_binary_op(&mut self, lhs: &Node, rhs: &Node, op: &BinOp, insts: &mut ByteCode) {
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
            &BinOp::Lt => self.bytecode_gen.gen_lt(insts),
            &BinOp::Gt => self.bytecode_gen.gen_gt(insts),
            &BinOp::Le => self.bytecode_gen.gen_le(insts),
            &BinOp::Ge => self.bytecode_gen.gen_ge(insts),
            _ => {}
        }
    }

    pub fn run_assign(&mut self, dst: &Node, src: &Node, insts: &mut ByteCode) {
        self.run(src, insts);

        match dst {
            &Node::Identifier(ref name) => {
                if let Some(p) = self.local_varmap.last().unwrap().get(name.as_str()) {
                    self.bytecode_gen.gen_set_local(*p as u32, insts);
                } else {
                    self.bytecode_gen.gen_set_global(name.clone(), insts);
                }
            }
            &Node::Member(ref parent, ref member) => {
                self.run(&*parent, insts);
                self.bytecode_gen
                    .gen_push_const(Value::String(member.clone()), insts);
                self.bytecode_gen.gen_set_member(insts);
            }
            &Node::Index(ref parent, ref idx) => {
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
                        .gen_push_const(Value::String(name.clone()), insts);
                }
            }
        }

        self.bytecode_gen
            .gen_create_object(properties.len() as usize, insts);
    }
}

impl VMCodeGen {
    fn run_member(&mut self, parent: &Node, member: &String, insts: &mut ByteCode) {
        self.run(parent, insts);

        self.bytecode_gen
            .gen_push_const(Value::String(member.clone()), insts);
        self.bytecode_gen.gen_get_member(insts);
    }

    fn run_index(&mut self, parent: &Node, idx: &Node, insts: &mut ByteCode) {
        self.run(parent, insts);

        self.run(idx, insts);
        self.bytecode_gen.gen_get_member(insts);
    }

    fn run_identifier(&mut self, name: &String, insts: &mut ByteCode) {
        if let Some(p) = self.local_varmap.last().unwrap().get(name.as_str()) {
            self.bytecode_gen.gen_get_local(*p as u32, insts);
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
