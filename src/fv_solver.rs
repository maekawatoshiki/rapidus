use node::{Node, NodeBase, PropertyDefinition};

use rand::random;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct FreeVariableSolver {
    pub cur_fv: HashSet<String>,
    pub mangled_name: Vec<HashMap<String, String>>,
    pub use_this: bool,
}

impl FreeVariableSolver {
    pub fn new() -> FreeVariableSolver {
        FreeVariableSolver {
            cur_fv: HashSet::new(),
            mangled_name: vec![],
            use_this: false,
        }
    }

    pub fn run_toplevel(&mut self, node: &mut Node) {
        match &mut node.base {
            &mut NodeBase::StatementList(ref mut nodes) => {
                let mut func_decl_index = vec![];
                let mut map = HashMap::new();
                for (i, node) in nodes.iter_mut().enumerate() {
                    if let &mut NodeBase::FunctionDecl(_, _, ref mut fv, _, _) = &mut node.base {
                        for name in fv.iter() {
                            map.insert(
                                name.clone(),
                                format!("{}.{}", name.clone(), random::<u32>()),
                            );
                        }
                        fv.clear();
                        func_decl_index.push(i)
                    }
                }
                self.mangled_name.push(map);

                for index in func_decl_index {
                    self.run(&mut nodes[index])
                }

                for node in nodes.iter_mut() {
                    match &mut node.base {
                        &mut NodeBase::FunctionDecl(_, _, _, _, _) => {}
                        _ => self.run(node),
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn run(&mut self, node: &mut Node) {
        let mut node_cloned = node.clone();
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

                let mut map = HashMap::new();
                for node in body.iter_mut() {
                    if let &mut NodeBase::FunctionDecl(_, _, ref mut fv, _, _) = &mut node.base {
                        for name in fv.iter() {
                            if self.get_mangled_name(name.as_str()).is_none() {
                                map.insert(
                                    name.clone(),
                                    format!("{}.{}", name.clone(), random::<u32>()),
                                );
                            }
                        }
                        fv.clear();
                    }
                }
                self.mangled_name.push(map);

                for node in body.iter_mut() {
                    self.run(node)
                }

                self.mangled_name.pop();
            }
            NodeBase::Call(ref mut callee, ref mut args) => {
                self.run(callee);
                for arg in args {
                    self.run(arg)
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
            NodeBase::This => self.use_this = true,
            NodeBase::Identifier(ref mut name) => {
                if let Some(name_) = self.get_mangled_name(name.as_str()) {
                    *name = name_;
                }
            }
            NodeBase::Object(ref mut properties) => {
                for property in properties.iter_mut() {
                    match property {
                        &mut PropertyDefinition::IdentifierReference(_) => unreachable!(),
                        &mut PropertyDefinition::Property(_, ref mut node) => self.run(node),
                    }
                }
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
            NodeBase::Assign(ref mut dst, ref mut src) => {
                self.run(&mut *dst);
                self.run(&mut *src);
            }
            NodeBase::VarDecl(_, _) => {
                if let NodeBase::VarDecl(ref name, ref mut init) = node_cloned.base {
                    if let Some(name) = self.get_mangled_name(name.as_str()) {
                        node.base = NodeBase::Assign(
                            Box::new(Node::new(NodeBase::Identifier(name), 0)),
                            if let &mut Some(ref mut init) = init {
                                self.run(init);
                                init.clone()
                            } else {
                                Box::new(Node::new(NodeBase::Number(0.0), 0))
                            },
                        );
                    }
                }
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
            _ => {}
        }
    }

    fn get_mangled_name(&self, name: &str) -> Option<String> {
        for map in self.mangled_name.iter().rev() {
            for (before_mangled, after_mangled) in map {
                if before_mangled == name {
                    return Some(after_mangled.clone());
                }
            }
        }
        None
    }
}
