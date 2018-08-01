use node::{Node, PropertyDefinition};

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
        match node {
            &mut Node::StatementList(ref mut nodes) => {
                let mut func_decl_index = vec![];
                let mut map = HashMap::new();
                for (i, node) in nodes.iter_mut().enumerate() {
                    if let &mut Node::FunctionDecl(_, _, ref mut fv, _, _) = node {
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
                    match node {
                        &mut Node::FunctionDecl(_, _, _, _, _) => {}
                        _ => self.run(node),
                    }
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

                let mut map = HashMap::new();
                for node in body.iter_mut() {
                    if let &mut Node::FunctionDecl(_, _, ref mut fv, _, _) = node {
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
            &mut Node::Call(ref mut callee, ref mut args) => {
                self.run(callee);
                for arg in args {
                    self.run(arg)
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
            &mut Node::Index(ref mut parent, ref mut idx) => {
                self.run(&mut *parent);
                self.run(&mut *idx);
            }
            &mut Node::This => self.use_this = true,
            &mut Node::Identifier(ref mut name) => {
                if let Some(name_) = self.get_mangled_name(name.as_str()) {
                    *name = name_;
                }
            }
            &mut Node::Object(ref mut properties) => {
                for property in properties.iter_mut() {
                    match property {
                        &mut PropertyDefinition::IdentifierReference(_) => unreachable!(),
                        &mut PropertyDefinition::Property(_, ref mut node) => self.run(node),
                    }
                }
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
            &mut Node::Assign(ref mut dst, ref mut src) => {
                self.run(&mut *dst);
                self.run(&mut *src);
            }
            &mut Node::VarDecl(_, _) => {
                if let Node::VarDecl(ref name, ref mut init) = node.clone() {
                    if let Some(name) = self.get_mangled_name(name.as_str()) {
                        *node = Node::Assign(
                            Box::new(Node::Identifier(name)),
                            if let &mut Some(ref mut init) = init {
                                self.run(init);
                                init.clone()
                            } else {
                                Box::new(Node::Number(0.0))
                            },
                        )
                    }
                }
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
