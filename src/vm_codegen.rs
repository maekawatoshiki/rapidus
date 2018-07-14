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
pub struct ClosureInfo {
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

impl ClosureInfo {
    pub fn new(
        name: String,
        use_this: bool,
        fv_name: Vec<String>,
        insts: Vec<Inst>,
    ) -> ClosureInfo {
        ClosureInfo {
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
    pub pending_closure_functions: HashMap<String, ClosureInfo>,
    pub local_var_stack_addr: IdGen,
    pub fv: Vec<Vec<String>>,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: HashMap::new(),
            local_varmap: vec![HashMap::new()],
            functions: HashMap::new(),
            pending_closure_functions: HashMap::new(),
            local_var_stack_addr: IdGen::new(),
            fv: vec![vec![]],
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, insts: &mut Vec<Inst>) {
        let pos = insts.len();
        insts.push(Inst::AllocLocalVar(0, 0));

        self.run(node, insts);

        if let Inst::AllocLocalVar(ref mut n, _) = insts[pos] {
            *n = self.local_var_stack_addr.get_cur_id()
        }
        insts.push(Inst::End);

        self.process_pending_functions();

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
                    *mem = Value::Cls(
                        Box::new(Value::Function(pos)),
                        *use_this,
                        fv_stack_addr.clone(),
                    );
                    self.global_varmap.insert(name.clone(), mem);
                } else {
                    *mem = Value::Function(pos);
                    self.global_varmap.insert(name.clone(), mem);
                }
            }

            let mut func_insts = func_insts.clone();
            insts.append(&mut func_insts);
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
        name: &Option<String>,
        use_this: bool,
        fv: &HashSet<String>,
        params: &FormalParameters,
        body: &Node,
    ) {
        let name = name.clone().unwrap();

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
            ClosureInfo::new(
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
        insts.push(Inst::NewThis);
        self.run(expr, insts);
        insts.push(Inst::DumpThis);
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
        self.run(callee, insts);

        for arg in args {
            self.run(arg, insts);
        }

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

pub fn test() {
    use parser::Parser;
    use vm::VM;
    let mut node_list = vec![];
    let mut parser = Parser::new("a = 123.456; console.log(a)".to_string());
    while let Ok(ok) = parser.next() {
        node_list.push(ok)
    }
    let mut vm_codegen = VMCodeGen::new();
    let mut insts = vec![];
    vm_codegen.compile(&node_list[0], &mut insts);
    for inst in &insts {
        println!("{:?}", inst);
    }

    println!("VM Test:");
    let mut vm = VM::new();
    vm.run(insts);
}
