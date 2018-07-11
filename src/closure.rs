use node::{FormalParameter, FormalParameters, Node};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FV {
    fv: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ClosureConv {
    pub varmap: Vec<HashMap<String, Option<FV>>>,
    pub cur_fv: Vec<String>,
    pub funcs: Vec<(Node, Vec<String>)>,
}

impl ClosureConv {
    pub fn new() -> ClosureConv {
        ClosureConv {
            varmap: vec![HashMap::new()],
            cur_fv: vec![],
            funcs: vec![],
        }
    }

    pub fn run(&mut self, input_nodes: &Vec<Node>, output_nodes: &mut Vec<Node>) {
        for node in input_nodes {
            match node {
                Node::StatementList(nodes) => {
                    let mut a = vec![];
                    self.run(nodes, &mut a);
                    output_nodes.push(Node::StatementList(a));
                }
                Node::FunctionDecl(name, fv, params, body) => self.run_func_decl(
                    name.clone(),
                    fv.clone(),
                    params.clone(),
                    &*body.clone(),
                    input_nodes,
                    output_nodes,
                ),
                Node::Call(callee, args) => {
                    self.run_call(&*callee.clone(), args.clone(), input_nodes, output_nodes)
                }
                Node::VarDecl(name, init) => {
                    self.varmap.last_mut().unwrap().insert(name.clone(), None);
                    output_nodes.push(Node::VarDecl(name.clone(), init.clone()))
                }
                Node::Return(val) => {
                    if let Some(val) = val {
                        let mut a = vec![];
                        self.run(&vec![*val.clone()], &mut a);
                        output_nodes.push(Node::Return(Some(Box::new(a[0].clone()))));
                    } else {
                        output_nodes.push(Node::Return(None))
                    }
                }
                Node::Identifier(name) => {
                    if !self.varmap.last().unwrap().contains_key(name.as_str()) {
                        self.cur_fv.push(name.clone());
                    }
                    output_nodes.push(Node::Identifier(name.clone()))
                }
                _ => output_nodes.push(node.clone()),
            }
        }
    }

    fn run_func_decl(
        &mut self,
        name: Option<String>,
        fv: Vec<String>,
        params: FormalParameters,
        body: &Node,
        input_nodes: &Vec<Node>,
        output_nodes: &mut Vec<Node>,
    ) {
        let nest = self.varmap.len() > 1;
        self.varmap.push(HashMap::new());

        for param in params.clone() {
            self.varmap.last_mut().unwrap().insert(param.name, None);
        }

        let mut newbody = vec![];
        self.run(&vec![body.clone()], &mut newbody);

        let mut newparams = vec![];
        for a in params {
            newparams.push(a);
        }

        let mut flg = false;
        if nest {
            self.funcs.push((
                Node::FunctionDecl(name, self.cur_fv.clone(), newparams, Box::new(Node::StatementList(newbody))),
                self.cur_fv.clone(),
            ));
            self.cur_fv.clear();
            flg = true;
        } else {
            output_nodes.push(Node::FunctionDecl(
                name,
                vec![],
                newparams,
                Box::new(Node::StatementList(newbody)),
            ));
            for (a, fv) in self.funcs.clone() {
                output_nodes.push(a);
            }
            self.funcs.clear()
        }

        self.varmap.pop();
    }

    fn run_call(
        &mut self,
        callee: &Node,
        args: Vec<Node>,
        input_nodes: &Vec<Node>,
        output_nodes: &mut Vec<Node>,
    ) {
        let mut new_args = vec![];

        // if let &Node::Identifier(ref name) = callee {
        //     if let Some(fv) = self.varmap.last().unwrap().get(name.as_str()) {
        //         for v in fv.clone() {
        //             new_args.push(Node::Identifier(v))
        //         }
        //     }
        // }

        for arg in args {
            if let Node::Identifier(name) = arg.clone() {
                if !self.varmap.last().unwrap().contains_key(name.as_str()) {
                    self.cur_fv.push(name);
                }
            }
            new_args.push(arg)
        }
        output_nodes.push(Node::Call(Box::new(callee.clone()), new_args));
    }
}
