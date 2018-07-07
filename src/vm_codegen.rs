use node::{BinOp, Node};
use vm::{Inst, Value};

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct VMCodeGen {
    pub global_varmap: HashMap<String, usize>, // usize will be replaced with an appropriate type
    pub local_varmap: Vec<HashMap<String, usize>>,
    pub id: usize,
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: HashMap::new(),
            local_varmap: vec![HashMap::new()],
            id: 0,
        }
    }
}

impl VMCodeGen {
    pub fn compile(&mut self, node: &Node, insts: &mut Vec<Inst>) {
        let pos = insts.len();
        insts.push(Inst::AllocLocalVar(0));

        self.run(node, insts);

        if let Inst::AllocLocalVar(ref mut n) = insts[pos] {
            *n = self.id
        }
    }

    fn run(&mut self, node: &Node, insts: &mut Vec<Inst>) {
        match node {
            &Node::StatementList(ref node_list) => self.run_statement_list(node_list, insts),
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
            &Node::Identifier(ref name) => self.run_identifier(name, insts),
            &Node::String(ref s) => insts.push(Inst::Push(Value::String(s.clone()))),
            &Node::Number(n) => insts.push(Inst::Push(Value::Number(n))),
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
    pub fn run_var_decl(&mut self, name: &String, init: &Option<Box<Node>>, insts: &mut Vec<Inst>) {
        self.local_varmap
            .last_mut()
            .unwrap()
            .insert(name.clone(), self.id);

        if let &Some(ref init) = init {
            self.run(&*init, insts);
            insts.push(Inst::SetLocal(self.id));
        }
        self.id += 1;
    }
}

impl VMCodeGen {
    pub fn run_if(&mut self, cond: &Node, then_: &Node, else_: &Node, insts: &mut Vec<Inst>) {
        self.run(cond, insts);

        let cond_pos = insts.len();
        insts.push(Inst::JmpIfFalse(0));

        self.run(then_, insts);

        if else_ == &Node::Nope {
            let pos = insts.len();
            if let Inst::JmpIfFalse(ref mut dst) = insts[cond_pos] {
                *dst = pos
            }
        } else {
            let then_end_pos = insts.len();
            insts.push(Inst::Jmp(0));

            let pos = insts.len();
            if let Inst::JmpIfFalse(ref mut dst) = insts[cond_pos] {
                *dst = pos
            }

            self.run(else_, insts);

            let pos = insts.len();
            if let Inst::Jmp(ref mut dst) = insts[then_end_pos] {
                *dst = pos
            }
        }
    }

    pub fn run_while(&mut self, cond: &Node, body: &Node, insts: &mut Vec<Inst>) {
        let pos = insts.len();

        self.run(cond, insts);

        let cond_pos = insts.len();
        insts.push(Inst::JmpIfFalse(0));

        self.run(body, insts);

        insts.push(Inst::Jmp(pos));

        let pos = insts.len();
        if let Inst::JmpIfFalse(ref mut dst) = insts[cond_pos] {
            *dst = pos
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

        insts.push(Inst::Push(Value::Data(member.clone())));
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
