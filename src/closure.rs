use node::{FormalParameter, FormalParameters, Node};

use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct ClosureConv {
    pub varmap: Vec<HashSet<String>>,
    pub cur_fv: HashSet<String>,
}

impl ClosureConv {
    pub fn new() -> ClosureConv {
        ClosureConv {
            varmap: vec![HashSet::new()],
            cur_fv: HashSet::new(),
        }
    }

    pub fn run(&mut self, node: &mut Node) {
        match node {
            &mut Node::StatementList(ref mut nodes) => {
                for node in nodes {
                    self.run(node)
                }
            }
            &mut Node::FunctionDecl(ref name, ref mut fv, ref params, ref mut body) => {
                self.varmap.push(HashSet::new());
                if let &Some(ref name) = name {
                    self.varmap.last_mut().unwrap().insert(name.clone());
                }

                for param in params.clone() {
                    self.varmap.last_mut().unwrap().insert(param.name);
                }

                self.run(body);

                for v in self.varmap.last().unwrap() {
                    self.cur_fv.remove(v);
                }

                *fv = self.cur_fv.clone();

                self.varmap.pop();
                if let &Some(ref name) = name {
                    self.varmap.last_mut().unwrap().insert(name.clone());
                }
            }
            &mut Node::Call(ref mut callee, ref mut args) => {
                self.run(callee);
                for arg in args {
                    self.run(arg)
                }
            }
            &mut Node::VarDecl(ref name, ref init) => {
                self.varmap.last_mut().unwrap().insert(name.clone());
            }
            &mut Node::Return(ref mut val) => {
                if let &mut Some(ref mut val) = val {
                    self.run(&mut **val)
                }
            }
            &mut Node::Identifier(ref name) => {
                if !self.varmap.last().unwrap().contains(name.as_str()) {
                    self.cur_fv.insert(name.clone());
                }
            }
            _ => {}
        }
    }
}
