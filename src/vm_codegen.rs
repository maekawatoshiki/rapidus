use id::IdGen;
use node::{BinOp, FormalParameters, Node};
use std::collections::HashSet;
use vm::{alloc_for_value, alloc_rawstring, HeapAddr, Inst, Value};

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub use_this: bool,
    pub fv_stack_addr: Vec<usize>,
    pub insts: Vec<Inst>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PendingFunctionInfo {
    pub name: String,
    pub use_this: bool,
    pub fv_name: Vec<String>,
    pub insts: Vec<Inst>,
}

impl FunctionInfo {
    pub fn new(
        name: String,
        use_this: bool,
        fv_stack_addr: Vec<usize>,
        insts: Vec<Inst>,
    ) -> FunctionInfo {
        FunctionInfo {
            name: name,
            use_this: use_this,
            insts: insts,
            fv_stack_addr: fv_stack_addr,
        }
    }
}

impl PendingFunctionInfo {
    pub fn new(
        name: String,
        use_this: bool,
        fv_name: Vec<String>,
        insts: Vec<Inst>,
    ) -> PendingFunctionInfo {
        PendingFunctionInfo {
            name: name,
            use_this: use_this,
            insts: insts,
            fv_name: fv_name,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VMCodeGen {
    pub global_varmap: HashMap<String, HeapAddr>, // usize will be replaced with an appropriate type
    pub local_varmap: Vec<HashMap<String, usize>>,
    pub functions: HashMap<String, FunctionInfo>,
    pub pending_closure_functions: HashMap<String, PendingFunctionInfo>,
    pub local_var_stack_addr: IdGen,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: HashMap::new(),
            local_varmap: vec![HashMap::new()],
            functions: HashMap::new(),
            pending_closure_functions: HashMap::new(),
            local_var_stack_addr: IdGen::new(),
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, insts: &mut Vec<Inst>) {
        let pos = insts.len();
        insts.push(Inst::AllocLocalVar(0, 0));

        self.run_var_decl2(&"this".to_string(), &None, insts);

        self.run(node, insts);

        if let Inst::AllocLocalVar(ref mut n, ref mut argc) = insts[pos] {
            *n = self.local_var_stack_addr.get_cur_id() - 1/*for this*/;
            *argc = 1; // for 'this'
        }
        insts.push(Inst::End);

        self.process_pending_functions();

        let mut function_value_list = HashMap::new();

        for (
            _,
            FunctionInfo {
                name,
                use_this,
                insts: func_insts,
                fv_stack_addr,
            },
        ) in &self.functions
        {
            let pos = insts.len();
            unsafe {
                let mem = alloc_for_value();
                if fv_stack_addr.len() > 0 || *use_this {
                    *mem = Value::MakeCls(
                        Box::new(Value::Function(pos)),
                        *use_this,
                        fv_stack_addr.clone(),
                    );
                    self.global_varmap.insert(name.clone(), mem);
                } else {
                    *mem = Value::Function(pos);
                    self.global_varmap.insert(name.clone(), mem);
                }
                function_value_list.insert(name.clone(), (*mem).clone());
            }

            let mut func_insts = func_insts.clone();
            insts.append(&mut func_insts);
        }

        for inst in insts.iter_mut() {
            let val = match inst {
                Inst::GetGlobal(name) => function_value_list.get(name),
                _ => None,
            };

            if let Some(val) = val {
                match val {
                    Value::MakeCls(callee, use_this, fv_addrs) => {
                        *inst = Inst::PushMakeCls(callee.clone(), *use_this, fv_addrs.clone())
                    }
                    _ => *inst = Inst::Push(val.clone()),
                }
            }
        }
    }

    fn run(&mut self, node: &Node, insts: &mut Vec<Inst>) {
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
            &Node::Return(ref val) => self.run_return(val, insts),
            &Node::New(ref expr) => self.run_new_expr(&*expr, insts),
            &Node::Identifier(ref name) => self.run_identifier(name, insts),
            &Node::This => insts.push(Inst::PushThis),
            &Node::String(ref s) => insts.push(Inst::Push(Value::String(unsafe {
                alloc_rawstring(s.as_str())
            }))),
            &Node::Number(n) => insts.push(Inst::Push(Value::Number(n))),
            &Node::Boolean(b) => insts.push(Inst::Push(Value::Bool(b))),
            _ => {}
        }
    }
}

impl VMCodeGen {
    pub fn run_statement_list(&mut self, node_list: &Vec<Node>, insts: &mut Vec<Inst>) {
        for node in node_list {
            self.run(node, insts)
        }
    }
}

impl VMCodeGen {
    fn process_pending_functions(&mut self) {
        for (name, v) in self.pending_closure_functions.clone() {
            let mut names = v.fv_name.clone();
            let mut fv_stack_addr = vec![];
            for name in names {
                if let Some(p) = self.local_varmap.last().unwrap().get(name.as_str()) {
                    fv_stack_addr.push(*p);
                } else {
                    unreachable!()
                };
            }
            self.functions.insert(
                name,
                FunctionInfo::new(v.name, v.use_this, fv_stack_addr, v.insts),
            );
        }
        self.pending_closure_functions.clear();
    }

    pub fn run_function_decl(
        &mut self,
        name: &String,
        use_this: bool,
        fv: &HashSet<String>,
        params: &FormalParameters,
        body: &Node,
    ) {
        let name = name.clone();

        self.local_varmap.push(HashMap::new());
        self.local_var_stack_addr.save();

        let mut func_insts = vec![];

        func_insts.push(Inst::AllocLocalVar(0, 0));

        if use_this {
            self.run_var_decl2(&"this".to_string(), &None, &mut func_insts);
        }

        for name in fv {
            self.run_var_decl2(name, &None, &mut func_insts);
        }

        for param in params {
            self.run_var_decl2(&param.name, &param.init, &mut func_insts)
        }

        let params_len = params.len() + fv.len() + if use_this { 1 } else { 0 };

        self.run(body, &mut func_insts);

        match func_insts.last() {
            Some(&Inst::Return) => {}
            _ => {
                func_insts.push(Inst::Push(Value::Undefined));
                func_insts.push(Inst::Return)
            }
        }

        if let Inst::AllocLocalVar(ref mut n, ref mut argc) = func_insts[0] {
            *n = self.local_var_stack_addr.get_cur_id() - params_len;
            *argc = params_len;
        }

        self.process_pending_functions();

        self.local_var_stack_addr.restore();
        self.local_varmap.pop();

        self.pending_closure_functions.insert(
            name.clone(),
            PendingFunctionInfo::new(
                name.clone(),
                use_this,
                fv.iter().cloned().collect::<Vec<_>>(),
                func_insts,
            ),
        );
    }

    pub fn run_return(&mut self, val: &Option<Box<Node>>, insts: &mut Vec<Inst>) {
        if let &Some(ref val) = val {
            self.run(&*val, insts)
        } else {
            insts.push(Inst::Push(Value::Undefined));
        }
        insts.push(Inst::Return);
    }
}

impl VMCodeGen {
    pub fn run_new_expr(&mut self, expr: &Node, insts: &mut Vec<Inst>) {
        insts.push(Inst::CreateThis);
        self.run(expr, insts);
        insts.push(Inst::DumpCurrentThis);
    }
}

impl VMCodeGen {
    pub fn run_var_decl(&mut self, name: &String, init: &Option<Box<Node>>, insts: &mut Vec<Inst>) {
        let id = self.local_var_stack_addr.gen_id();

        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), id);

        if let &Some(ref init) = init {
            self.run(&*init, insts);
            insts.push(Inst::SetLocal(id));
        }
    }

    pub fn run_var_decl2(&mut self, name: &String, init: &Option<Node>, insts: &mut Vec<Inst>) {
        let id = self.local_var_stack_addr.gen_id();

        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), id);

        if let &Some(ref init) = init {
            self.run(init, insts);
            insts.push(Inst::SetLocal(id));
        }
    }
}

impl VMCodeGen {
    pub fn run_if(&mut self, cond: &Node, then_: &Node, else_: &Node, insts: &mut Vec<Inst>) {
        self.run(cond, insts);

        let cond_pos = insts.len() as isize;
        insts.push(Inst::JmpIfFalse(0));

        self.run(then_, insts);

        if else_ == &Node::Nope {
            let pos = insts.len() as isize;
            if let Inst::JmpIfFalse(ref mut dst) = insts[cond_pos as usize] {
                *dst = pos - cond_pos
            }
        } else {
            let then_end_pos = insts.len() as isize;
            insts.push(Inst::Jmp(0));

            let pos = insts.len() as isize;
            if let Inst::JmpIfFalse(ref mut dst) = insts[cond_pos as usize] {
                *dst = pos - cond_pos
            }

            self.run(else_, insts);

            let pos = insts.len() as isize;
            if let Inst::Jmp(ref mut dst) = insts[then_end_pos as usize] {
                *dst = pos - then_end_pos
            }
        }
    }

    pub fn run_while(&mut self, cond: &Node, body: &Node, insts: &mut Vec<Inst>) {
        let pos = insts.len() as isize;

        self.run(cond, insts);

        let cond_pos = insts.len() as isize;
        insts.push(Inst::JmpIfFalse(0));

        self.run(body, insts);

        let loop_pos = insts.len() as isize;
        insts.push(Inst::Jmp(pos - loop_pos));

        let pos = insts.len() as isize;
        if let Inst::JmpIfFalse(ref mut dst) = insts[cond_pos as usize] {
            *dst = pos - cond_pos
        } else {
            unreachable!()
        }
    }
}

impl VMCodeGen {
    pub fn run_binary_op(&mut self, lhs: &Node, rhs: &Node, op: &BinOp, insts: &mut Vec<Inst>) {
        self.run(lhs, insts);
        self.run(rhs, insts);
        match op {
            &BinOp::Add => insts.push(Inst::Add),
            &BinOp::Sub => insts.push(Inst::Sub),
            &BinOp::Mul => insts.push(Inst::Mul),
            &BinOp::Div => insts.push(Inst::Div),
            &BinOp::Rem => insts.push(Inst::Rem),
            &BinOp::Eq => insts.push(Inst::Eq),
            &BinOp::Ne => insts.push(Inst::Ne),
            &BinOp::Lt => insts.push(Inst::Lt),
            &BinOp::Gt => insts.push(Inst::Gt),
            &BinOp::Le => insts.push(Inst::Le),
            &BinOp::Ge => insts.push(Inst::Ge),
            _ => {}
        }
    }

    pub fn run_assign(&mut self, dst: &Node, src: &Node, insts: &mut Vec<Inst>) {
        self.run(src, insts);

        match dst {
            &Node::Identifier(ref name) => {
                if let Some(p) = self.local_varmap.last().unwrap().get(name.as_str()) {
                    insts.push(Inst::SetLocal(*p));
                } else {
                    insts.push(Inst::SetGlobal(name.clone()));
                }
            }
            &Node::Member(ref parent, ref member) => {
                self.run(&*parent, insts);
                unsafe {
                    insts.push(Inst::Push(Value::String(alloc_rawstring(member.as_str()))));
                }

                insts.push(Inst::SetMember)
            }
            _ => {}
        }
    }
}

impl VMCodeGen {
    pub fn run_call(&mut self, callee: &Node, args: &Vec<Node>, insts: &mut Vec<Inst>) {
        for arg in args {
            self.run(arg, insts);
        }

        self.run(callee, insts);

        insts.push(Inst::Call(args.len()));
    }
}

impl VMCodeGen {
    fn run_member(&mut self, parent: &Node, member: &String, insts: &mut Vec<Inst>) {
        self.run(parent, insts);

        unsafe {
            insts.push(Inst::Push(Value::String(alloc_rawstring(member.as_str()))));
        }
        insts.push(Inst::GetMember)
    }

    fn run_identifier(&mut self, name: &String, insts: &mut Vec<Inst>) {
        if let Some(p) = self.local_varmap.last().unwrap().get(name.as_str()) {
            insts.push(Inst::GetLocal(*p))
        } else {
            insts.push(Inst::GetGlobal(name.clone()))
        }
    }
}

#[test]
fn binaryop() {
    use parser;
    for (op_s, op_i) in vec![
        ("+", Inst::Add),
        ("-", Inst::Sub),
        ("*", Inst::Mul),
        ("/", Inst::Div),
        ("%", Inst::Rem),
        ("<", Inst::Lt),
        ("<=", Inst::Le),
        (">", Inst::Gt),
        (">=", Inst::Ge),
        ("==", Inst::Eq),
        ("!=", Inst::Ne),
    ] {
        let mut output = vec![];
        VMCodeGen::new().compile(
            &parser::Parser::new(format!("1 {} 2", op_s)).next().unwrap(),
            &mut output,
        );
        assert_eq!(
            vec![
                Inst::AllocLocalVar(0, 1),
                Inst::Push(Value::Number(1.0)),
                Inst::Push(Value::Number(2.0)),
                op_i,
                Inst::End,
            ],
            output
        );
    }
}

#[test]
fn string() {
    use parser;
    let mut output = vec![];
    VMCodeGen::new().compile(
        &parser::Parser::new("\"hello\"".to_string()).next().unwrap(),
        &mut output,
    );

    // The address (of Value::String) cannot be compared...
    // unsafe {
    //     assert_eq!(
    //         vec![
    //             Inst::AllocLocalVar(0, 1),
    //             Inst::Push(Value::String(alloc_rawstring("hello"))), <<-- X(
    //             Inst::End,
    //         ],
    //         output
    //     );
    // }
}

#[test]
fn local_var_load() {
    use parser;
    let mut output = vec![];
    VMCodeGen::new().compile(
        &parser::Parser::new("var i; i".to_string()).next().unwrap(),
        &mut output,
    );
    assert_eq!(
        vec![Inst::AllocLocalVar(1, 1), Inst::GetLocal(1), Inst::End],
        output
    );
}

#[test]
fn local_var_assign() {
    use parser;
    let mut output = vec![];
    VMCodeGen::new().compile(
        &parser::Parser::new("var i; i = 123".to_string())
            .next()
            .unwrap(),
        &mut output,
    );
    assert_eq!(
        vec![
            Inst::AllocLocalVar(1, 1),
            Inst::Push(Value::Number(123.0)),
            Inst::SetLocal(1),
            Inst::End,
        ],
        output
    );
}

#[test]
fn global_func_call() {
    use parser;
    for (args_s, mut args_i) in vec![
        ("", vec![]),
        ("1", vec![Inst::Push(Value::Number(1.0))]),
        (
            "1, 2",
            vec![
                Inst::Push(Value::Number(1.0)),
                Inst::Push(Value::Number(2.0)),
            ],
        ),
    ] {
        let mut output = vec![];
        VMCodeGen::new().compile(
            &parser::Parser::new(format!("f({})", args_s))
                .next()
                .unwrap(),
            &mut output,
        );
        let mut expect = vec![Inst::AllocLocalVar(0, 1)];
        let args_len = args_i.len();
        expect.append(&mut args_i);
        expect.append(&mut vec![
            Inst::GetGlobal("f".to_string()),
            Inst::Call(args_len),
            Inst::End,
        ]);
        assert_eq!(expect, output);
    }
}

#[test]
fn member_load() {
    use parser;
    let mut output = vec![];
    VMCodeGen::new().compile(
        &parser::Parser::new("console.log".to_string())
            .next()
            .unwrap(),
        &mut output,
    );
    unsafe {
        assert_eq!(output[0], Inst::AllocLocalVar(0, 1));
        assert_eq!(output[1], Inst::GetGlobal("console".to_string()));
        if let Inst::Push(Value::String(s)) = output[2] {
            use std::ffi::CStr;
            assert!(CStr::from_ptr(s).to_str().unwrap() == "log")
        } else {
            panic!()
        }
        assert_eq!(output[3], Inst::GetMember);
        assert_eq!(output[4], Inst::End);
    }
}

#[test]
fn member_assign() {
    let mut output = vec![];
    // JS: a.s = 123;
    let node = Node::StatementList(vec![Node::Assign(
        Box::new(Node::Member(
            Box::new(Node::Identifier("a".to_string())),
            "s".to_string(),
        )),
        Box::new(Node::Number(123.0)),
    )]);
    VMCodeGen::new().compile(&node, &mut output);
    unsafe {
        assert_eq!(output[0], Inst::AllocLocalVar(0, 1));
        assert_eq!(output[1], Inst::Push(Value::Number(123.0)));
        assert_eq!(output[2], Inst::GetGlobal("a".to_string()));
        if let Inst::Push(Value::String(s)) = output[3] {
            use std::ffi::CStr;
            assert!(CStr::from_ptr(s).to_str().unwrap() == "s")
        } else {
            panic!()
        }
        assert_eq!(output[4], Inst::SetMember);
        assert_eq!(output[5], Inst::End);
    }
}

#[test]
fn while_() {
    let mut output = vec![];
    // JS: while(true) { }
    let node = Node::StatementList(vec![Node::While(
        Box::new(Node::Boolean(true)),
        Box::new(Node::StatementList(vec![])),
    )]);
    VMCodeGen::new().compile(&node, &mut output);
    assert_eq!(
        vec![
            Inst::AllocLocalVar(0, 1),
            Inst::Push(Value::Bool(true)),
            Inst::JmpIfFalse(2),
            Inst::Jmp(-2),
            Inst::End,
        ],
        output
    );
}

#[test]
fn if_() {
    let mut output = vec![];
    // JS: if(x < 3) ; else ;
    let node = Node::StatementList(vec![Node::If(
        Box::new(Node::BinaryOp(
            Box::new(Node::Identifier("x".to_string())),
            Box::new(Node::Number(3.0)),
            BinOp::Lt,
        )),
        Box::new(Node::Nope),
        Box::new(Node::Nope),
    )]);
    VMCodeGen::new().compile(&node, &mut output);
    assert_eq!(
        vec![
            Inst::AllocLocalVar(0, 1),
            Inst::GetGlobal("x".to_string()),
            Inst::Push(Value::Number(3.0)),
            Inst::Lt,
            Inst::JmpIfFalse(1),
            Inst::End,
        ],
        output
    );
}
