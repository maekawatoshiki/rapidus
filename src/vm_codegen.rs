use node::Node;
use vm::{Inst, Value};

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct VMCodeGen {
    pub global_varmap: HashMap<String, usize>, // usize will be replaced with an appropriate type
}

impl VMCodeGen {
    pub fn new() -> VMCodeGen {
        VMCodeGen {
            global_varmap: HashMap::new(),
        }
    }
}

impl VMCodeGen {
    pub fn run(&mut self, node: &Node, insts: &mut Vec<Inst>) {
        match node {
            &Node::StatementList(ref node_list) => self.run_statement_list(node_list, insts),
            &Node::Assign(ref dst, ref src) => self.run_assign(&*dst, &*src, insts),
            &Node::Call(ref callee, ref args) => self.run_call(&*callee, args, insts),
            &Node::Identifier(ref name) => self.run_identifier(name, insts),
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
    pub fn run_assign(&mut self, dst: &Node, src: &Node, insts: &mut Vec<Inst>) {
        self.run(src, insts);

        if let Some((name, _is_global)) = self.get_var(dst) {
            insts.push(Inst::SetGlobal(name));
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
    fn run_identifier(&mut self, name: &String, insts: &mut Vec<Inst>) {
        // if let Some(_) = self.global_varmap.get(name.as_str()) {
        insts.push(Inst::GetGlobal(name.clone()))
        // }
    }
}

impl VMCodeGen {
    fn get_var(&mut self, var: &Node) -> Option<(String, bool)> {
        match var {
            &Node::Identifier(ref name) => {
                // if let Some(_) = self.global_varmap.get(name.as_str()) {
                //     return Some((name.clone(), true));
                // }
                // self.global_varmap.insert(name.clone(), 0);
                Some((name.clone(), true))
            }
            _ => None,
        }
    }
}

pub fn test() {
    use parser::Parser;
    let mut node_list = vec![];
    let mut parser = Parser::new("a = 1; f(a)".to_string());
    while let Ok(ok) = parser.next() {
        node_list.push(ok)
    }
    let mut vm_codegen = VMCodeGen::new();
    let mut insts = vec![];
    vm_codegen.run(&node_list[0], &mut insts);
    for inst in insts {
        println!("{:?}", inst);
    }
}
