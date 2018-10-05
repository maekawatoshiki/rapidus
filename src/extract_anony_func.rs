use node::{Node, NodeBase, PropertyDefinition};

use rand::random;

#[derive(Debug, Clone)]
pub struct AnonymousFunctionExtractor {
    pub pending_function: Vec<Node>,
    pub nest: usize,
}

impl AnonymousFunctionExtractor {
    pub fn new() -> AnonymousFunctionExtractor {
        AnonymousFunctionExtractor {
            pending_function: vec![],
            nest: 0,
        }
    }

    pub fn run_toplevel(&mut self, node: &mut Node) {
        match &mut node.base {
            &mut NodeBase::StatementList(ref mut nodes) => {
                for node in nodes.iter_mut() {
                    self.run(node)
                }

                for pending_function in &self.pending_function {
                    nodes.push(pending_function.clone())
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
            NodeBase::FunctionDecl(_, _, _) => {
                if let NodeBase::FunctionDecl(ref name, ref params, ref mut body) =
                    node.clone().base
                {
                    // TODO: Need refinement
                    let mut body =
                        if let &mut NodeBase::StatementList(ref mut body) = &mut body.base {
                            body
                        } else {
                            unreachable!()
                        };

                    let mut name_mangled = format!("anonymous.{}.{}", name, random::<u32>());

                    self.nest += 1;
                    for node in body.iter_mut() {
                        self.run(node)
                    }
                    self.nest -= 1;

                    if self.nest > 0 {
                        self.pending_function.push(Node::new(
                            NodeBase::FunctionDecl(
                                name_mangled.clone(),
                                params.clone(),
                                Box::new(Node::new(NodeBase::StatementList(body.clone()), 0)),
                            ),
                            0,
                        ));

                        node.base = NodeBase::VarDecl(
                            name.clone(),
                            Some(Box::new(Node::new(
                                NodeBase::SetCurCallObj(name_mangled),
                                0,
                            ))),
                        );
                    } else {
                        if let NodeBase::FunctionDecl(_, _, ref mut body_) = node.base {
                            *body_ = Box::new(Node::new(NodeBase::StatementList(body.clone()), 0));
                        }
                    }
                }
            }
            NodeBase::FunctionExpr(_, _, _) => {
                if let NodeBase::FunctionExpr(mut name, mut params, mut body) = node.clone().base {
                    let mut name_ = match name {
                        Some(name) => {
                            let new_name = format!("anonymous.{}.{}", name, random::<u32>());
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

                    self.pending_function.push(Node::new(
                        NodeBase::FunctionDecl(
                            name_.clone(),
                            params,
                            Box::new(Node::new(NodeBase::StatementList(body), 0)),
                        ),
                        0,
                    ));
                    *node = Node::new(NodeBase::SetCurCallObj(name_), 0);
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
            NodeBase::For(ref mut init, ref mut cond, ref mut step, ref mut body) => {
                self.run(&mut *init);
                self.run(&mut *cond);
                self.run(&mut *step);
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
            NodeBase::Identifier(ref mut _ident) => {}
            NodeBase::Object(ref mut properties) => {
                for property in properties.iter_mut() {
                    let name_of_ident_ref =
                        if let PropertyDefinition::IdentifierReference(name) = property.clone() {
                            Some(name)
                        } else {
                            None
                        };
                    match property {
                        &mut PropertyDefinition::IdentifierReference(_) => {
                            let mut name_of_ident_ref = name_of_ident_ref.unwrap();
                            *property = PropertyDefinition::Property(
                                name_of_ident_ref.to_string(),
                                Node::new(
                                    NodeBase::Identifier(name_of_ident_ref),
                                    node.pos, // TODO: Is this correct?
                                ),
                            );
                        }
                        &mut PropertyDefinition::Property(_, ref mut node) => self.run(node),
                    }
                }
            }

            _ => {}
        }
    }
}
