use node::{Node, PropertyDefinition};

use rand::random;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct AnonymousFunctionExtractor {
    pub pending_anonymous_function: Vec<Vec<Node>>,
    pub mangled_anonymous_function_name: Vec<(String, String)>,
}

impl AnonymousFunctionExtractor {
    pub fn new() -> AnonymousFunctionExtractor {
        AnonymousFunctionExtractor {
            pending_anonymous_function: vec![],
            mangled_anonymous_function_name: vec![],
        }
    }

    pub fn run_toplevel(&mut self, node: &mut Node) {
        match node {
            &mut Node::StatementList(ref mut nodes) => {
                for node in nodes.iter_mut() {
                    self.run(node)
                }
            }
            _ => unreachable!(),
        }
    }

    fn run(&mut self, node: &mut Node) {
        match node {
            &mut Node::StatementList(ref mut nodes) => {
                for node in nodes {
                    self.run(node)
                }
            }
            &mut Node::FunctionDecl(_, _, _, _, ref mut body) => {
                let mut body = if let &mut Node::StatementList(ref mut body) = &mut **body {
                    body
                } else {
                    unreachable!()
                };

                self.pending_anonymous_function.push(vec![]);

                for node in body.iter_mut() {
                    self.run(node)
                }

                for pending_anonymous_function in self.pending_anonymous_function.last().unwrap() {
                    body.push(pending_anonymous_function.clone())
                }

                self.pending_anonymous_function.pop();
            }
            &mut Node::FunctionExpr(_, _, _) => {
                if let Node::FunctionExpr(mut name, mut params, mut body) = node.clone() {
                    let mut name_ = match name {
                        Some(name) => {
                            let new_name = format!("anonymous.{}.{}", name, random::<u32>());
                            self.mangled_anonymous_function_name
                                .push((name.clone(), new_name.clone()));
                            new_name
                        }
                        None => format!("anonymous.{}", random::<u32>()),
                    };

                    let mut body = if let Node::StatementList(body) = *body {
                        body
                    } else {
                        unreachable!()
                    };

                    for node in body.iter_mut() {
                        self.run(node)
                    }

                    self.mangled_anonymous_function_name.pop();

                    self.pending_anonymous_function
                        .last_mut()
                        .unwrap()
                        .push(Node::FunctionDecl(
                            name_.clone(),
                            false,
                            HashSet::new(),
                            params,
                            Box::new(Node::StatementList(body)),
                        ));
                    *node = Node::Identifier(name_);
                }
            }
            &mut Node::Call(ref mut callee, ref mut args) => {
                self.run(callee);
                for arg in args {
                    self.run(arg)
                }
            }
            &mut Node::New(ref mut expr) => self.run(expr),
            &mut Node::VarDecl(_, ref mut init) => {
                if let &mut Some(ref mut init) = init {
                    self.run(init)
                }
            }
            &mut Node::Return(ref mut val) => {
                if let &mut Some(ref mut val) = val {
                    self.run(&mut **val)
                }
            }
            &mut Node::Member(ref mut parent, _) => {
                self.run(&mut *parent);
            }
            &mut Node::If(ref mut cond, ref mut then, ref mut else_) => {
                self.run(&mut *cond);
                self.run(&mut *then);
                self.run(&mut *else_);
            }
            &mut Node::While(ref mut cond, ref mut body) => {
                self.run(&mut *cond);
                self.run(&mut *body);
            }
            &mut Node::Assign(_, ref mut src) => {
                self.run(&mut *src);
            }
            &mut Node::UnaryOp(ref mut expr, _) => {
                self.run(&mut *expr);
            }
            &mut Node::BinaryOp(ref mut lhs, ref mut rhs, _) => {
                self.run(&mut *lhs);
                self.run(&mut *rhs);
            }
            &mut Node::TernaryOp(ref mut cond, ref mut then, ref mut else_) => {
                self.run(&mut *cond);
                self.run(&mut *then);
                self.run(&mut *else_);
            }
            &mut Node::Identifier(ref mut ident) => {
                if let Some(name) = self.get_mangled_anonymous_function_name(ident.as_str()) {
                    *ident = name.clone();
                }
            }
            _ => {}
        }
    }

    fn get_mangled_anonymous_function_name(&self, name: &str) -> Option<&String> {
        for (before_mangled, after_mangled) in self.mangled_anonymous_function_name.iter().rev() {
            if before_mangled == name {
                return Some(after_mangled);
            }
        }
        None
    }
}
