use node::{Node, NodeBase, PropertyDefinition};

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
            pending_anonymous_function: vec![vec![]],
            mangled_anonymous_function_name: vec![],
        }
    }

    pub fn run_toplevel(&mut self, node: &mut Node) {
        match &mut node.base {
            &mut NodeBase::StatementList(ref mut nodes) => {
                for node in nodes.iter_mut() {
                    self.run(node)
                }

                for pending_anonymous_function in self.pending_anonymous_function.last().unwrap() {
                    nodes.push(pending_anonymous_function.clone())
                }
            }
            _ => unreachable!(),
        }
    }

    fn run(&mut self, node: &mut Node) {
        match node.base {
            NodeBase::StatementList(ref mut nodes) => {
                for node in nodes {
                    self.run(node)
                }
            }
            NodeBase::FunctionDecl(_, _, _, _, ref mut body) => {
                let mut body = if let &mut NodeBase::StatementList(ref mut body) = &mut body.base {
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
            NodeBase::FunctionExpr(_, _, _) => {
                if let NodeBase::FunctionExpr(mut name, mut params, mut body) = node.clone().base {
                    let mut name_ = match name {
                        Some(name) => {
                            let new_name = format!("anonymous.{}.{}", name, random::<u32>());
                            self.mangled_anonymous_function_name
                                .push((name.clone(), new_name.clone()));
                            new_name
                        }
                        None => format!("anonymous.{}", random::<u32>()),
                    };

                    let mut body = if let NodeBase::StatementList(body) = body.base {
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
                        .push(Node::new(
                            NodeBase::FunctionDecl(
                                name_.clone(),
                                false,
                                HashSet::new(),
                                params,
                                Box::new(Node::new(NodeBase::StatementList(body), 0)),
                            ),
                            0,
                        ));
                    *node = Node::new(NodeBase::Identifier(name_), 0);
                }
            }
            NodeBase::Call(ref mut callee, ref mut args) => {
                self.run(callee);
                for arg in args {
                    self.run(arg)
                }
            }
            NodeBase::New(ref mut expr) => self.run(expr),
            NodeBase::VarDecl(_, ref mut init) => {
                if let &mut Some(ref mut init) = init {
                    self.run(init)
                }
            }
            NodeBase::Return(ref mut val) => {
                if let &mut Some(ref mut val) = val {
                    self.run(&mut **val)
                }
            }
            NodeBase::Member(ref mut parent, _) => {
                self.run(&mut *parent);
            }
            NodeBase::Index(ref mut parent, ref mut idx) => {
                self.run(&mut *parent);
                self.run(&mut *idx);
            }
            NodeBase::If(ref mut cond, ref mut then, ref mut else_) => {
                self.run(&mut *cond);
                self.run(&mut *then);
                self.run(&mut *else_);
            }
            NodeBase::While(ref mut cond, ref mut body) => {
                self.run(&mut *cond);
                self.run(&mut *body);
            }
            NodeBase::Assign(_, ref mut src) => {
                self.run(&mut *src);
            }
            NodeBase::UnaryOp(ref mut expr, _) => {
                self.run(&mut *expr);
            }
            NodeBase::BinaryOp(ref mut lhs, ref mut rhs, _) => {
                self.run(&mut *lhs);
                self.run(&mut *rhs);
            }
            NodeBase::TernaryOp(ref mut cond, ref mut then, ref mut else_) => {
                self.run(&mut *cond);
                self.run(&mut *then);
                self.run(&mut *else_);
            }
            NodeBase::Identifier(ref mut ident) => {
                if let Some(name) = self.get_mangled_anonymous_function_name(ident.as_str()) {
                    *ident = name.clone();
                }
            }
            NodeBase::Object(ref mut properties) => {
                for property in properties.iter_mut() {
                    match property {
                        &mut PropertyDefinition::IdentifierReference(_) => {}
                        &mut PropertyDefinition::Property(_, ref mut node) => self.run(node),
                    }
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
