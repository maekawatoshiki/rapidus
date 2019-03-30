use bytecode_gen::{ByteCode, ByteCodeGenerator, VMInst};
use gc::MemoryAllocator;
use id::IdGen;
use node::{
    BinOp, FormalParameter, FormalParameters, IdentifierInfo, MethodDefinitionKind, Node, NodeBase,
    PropertyDefinition, UnaryOp, VarKind,
};
use parser::Error;
use rustc_hash::{FxHashMap, FxHashSet};
use vm::constant::{ConstantTable, SpecialProperties, SpecialPropertyKind};
use vm::jsvalue::function::{DestinationKind, Exception};
use vm::jsvalue::value::Value2;
use vm::jsvalue::{prototype, value};

#[derive(Clone, Debug)]
pub enum Level {
    Function {
        params: VarMap,
        varmap: VarMap,
        fv: FVSet,
    },
    Block {
        varmap: VarMap,
    },
}

#[derive(Debug)]
pub struct Analyzer {
    level: Vec<Level>,
    idgen: IdGen,
}

pub type VarMap = FxHashMap<String, Option<usize>>;
pub type FVSet = FxHashSet<String>;

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {
            level: vec![Level::Block {
                varmap: VarMap::default(),
            }],
            idgen: IdGen::new(),
        }
    }

    pub fn analyze(&mut self, node: Node) -> Result<Node, Error> {
        if let NodeBase::StatementList(stmts) = node.base {
            let mut new_stmts = vec![];
            for stmt in stmts {
                new_stmts.push(self.visit(stmt)?)
            }
            Ok(Node::new(NodeBase::StatementList(new_stmts), node.pos))
        } else {
            panic!()
        }
    }
}

impl Analyzer {
    fn collect_variable_declarations(&mut self, node: &Node) {
        let stmts = if let NodeBase::StatementList(ref stmts) = node.base {
            stmts
        } else if let NodeBase::For(ref init, _, _, _) = node.base {
            return self.collect_variable_declarations(&**init);
        } else {
            return;
        };
        let varmap = self.get_current_varmap_mut();
        for stmt in stmts {
            if let NodeBase::VarDecl(ref name, _, _) = stmt.base {
                varmap.insert(name.clone(), None);
            }
        }
    }

    fn replace_variable_declarations(
        &mut self,
        node: &mut Node,
        bound_variables: &FxHashMap<String, usize>,
    ) {
        let stmts = if let NodeBase::StatementList(ref mut stmts) = node.base {
            stmts
        } else if let NodeBase::For(ref mut init, _, _, _) = node.base {
            return self.replace_variable_declarations(&mut **init, bound_variables);
        } else {
            return;
        };
        for stmt in stmts {
            if let NodeBase::VarDecl(name, init, _) = stmt.base.clone() {
                if let Some(offset) = bound_variables.get(&name) {
                    if let Some(init) = init {
                        stmt.base = NodeBase::Assign(
                            Box::new(Node::new(
                                NodeBase::Identifier(IdentifierInfo::Offset(*offset)),
                                0,
                            )),
                            init,
                        );
                    }
                }
            }
        }
    }

    fn visit(&mut self, node: Node) -> Result<Node, Error> {
        match node.base {
            NodeBase::StatementList(stmts) => {
                if self.in_function_level() {
                    for stmt in &stmts {
                        self.collect_variable_declarations(stmt);
                    }

                    let mut new_stmts = vec![];
                    for stmt in stmts {
                        new_stmts.push(self.visit(stmt)?)
                    }

                    let mut bound_variables = FxHashMap::default();
                    for (name, offset) in self.get_current_varmap() {
                        if let Some(offset) = offset {
                            bound_variables.insert(name.clone(), *offset);
                        }
                    }

                    for stmt in &mut new_stmts {
                        self.replace_variable_declarations(stmt, &bound_variables);
                    }

                    Ok(Node::new(NodeBase::StatementList(new_stmts), node.pos))
                } else {
                    Ok(Node::new(
                        NodeBase::StatementList({
                            let mut new_stmts = vec![];
                            for stmt in stmts {
                                new_stmts.push(self.visit(stmt)?)
                            }
                            new_stmts
                        }),
                        node.pos,
                    ))
                }
            }
            NodeBase::Block(stmts) => {
                if self.in_function_level() {
                    self.push_new_block_level();

                    for stmt in &stmts {
                        self.collect_variable_declarations(stmt);
                    }

                    let mut new_stmts = vec![];
                    for stmt in stmts {
                        new_stmts.push(self.visit(stmt)?)
                    }

                    let level = self.pop_level();
                    let varmap = level.get_varmap();
                    let mut bound_variables: FxHashMap<String, usize> = FxHashMap::default();
                    let mut has_free_variables = false;
                    for (name, offset) in varmap {
                        if let Some(offset) = offset {
                            bound_variables.insert(name.clone(), *offset);
                        } else {
                            has_free_variables |= true;
                        }
                    }

                    for stmt in &mut new_stmts {
                        self.replace_variable_declarations(stmt, &bound_variables);
                    }

                    Ok(Node::new(
                        if has_free_variables {
                            NodeBase::Block(new_stmts)
                        } else {
                            NodeBase::StatementList(new_stmts)
                        },
                        node.pos,
                    ))
                } else {
                    Ok(Node::new(
                        NodeBase::Block({
                            let mut new_stmts = vec![];
                            for stmt in stmts {
                                new_stmts.push(self.visit(stmt)?)
                            }
                            new_stmts
                        }),
                        node.pos,
                    ))
                }
            }
            NodeBase::If(cond, then, else_) => Ok(Node::new(
                NodeBase::If(
                    Box::new(self.visit(*cond)?),
                    Box::new(self.visit(*then)?),
                    Box::new(self.visit(*else_)?),
                ),
                node.pos,
            )),
            NodeBase::While(cond, body) => Ok(Node::new(
                NodeBase::While(Box::new(self.visit(*cond)?), Box::new(self.visit(*body)?)),
                node.pos,
            )),
            NodeBase::For(init, cond, step, body) => Ok(Node::new(
                NodeBase::For(
                    Box::new(self.visit(*init)?),
                    Box::new(self.visit(*cond)?),
                    Box::new(self.visit(*step)?),
                    Box::new(self.visit(*body)?),
                ),
                node.pos,
            )),
            NodeBase::Try(try, catch, param, finally) => Ok(Node::new(
                NodeBase::Try(
                    Box::new(self.visit(*try)?),
                    Box::new(self.visit(*catch)?),
                    Box::new(self.visit(*param)?),
                    Box::new(self.visit(*finally)?),
                ),
                node.pos,
            )),
            NodeBase::FunctionDecl {
                name,
                mut params,
                body,
                ..
            } => {
                self.idgen.save();
                let mut params_varmap = VarMap::default();
                for FormalParameter { ref name, .. } in &params {
                    params_varmap.insert(name.clone(), None);
                }
                self.push_new_function_level(params_varmap);

                let body = Box::new(self.visit(*body)?);
                let (params_varmap, _, fv) = self.pop_level().as_function_level();
                println!("fv {:?}", fv);

                for FormalParameter {
                    ref name,
                    ref mut bound,
                    ..
                } in &mut params
                {
                    if let Some(offset) = params_varmap.get(name).unwrap() {
                        *bound = Some(*offset);
                    }
                }

                let bound_variables = self.idgen.get_cur_id();
                self.idgen.restore();

                Ok(Node::new(
                    NodeBase::FunctionDecl {
                        name,
                        params,
                        body,
                        bound_variables,
                    },
                    node.pos,
                ))
            }
            NodeBase::FunctionExpr {
                name,
                mut params,
                body,
                ..
            } => {
                self.idgen.save();
                let mut params_varmap = VarMap::default();
                for FormalParameter { ref name, .. } in &params {
                    params_varmap.insert(name.clone(), None);
                }
                self.push_new_function_level(params_varmap);

                let body = Box::new(self.visit(*body)?);
                let (params_varmap, _, fv) = self.pop_level().as_function_level();
                println!("fv {:?}", fv);

                for FormalParameter {
                    ref name,
                    ref mut bound,
                    ..
                } in &mut params
                {
                    if let Some(offset) = params_varmap.get(name).unwrap() {
                        *bound = Some(*offset);
                    }
                }

                let bound_variables = self.idgen.get_cur_id();
                self.idgen.restore();

                Ok(Node::new(
                    NodeBase::FunctionExpr {
                        name,
                        params,
                        body,
                        bound_variables,
                    },
                    node.pos,
                ))
            }
            NodeBase::ArrowFunction {
                mut params, body, ..
            } => {
                self.idgen.save();

                let mut params_varmap = VarMap::default();
                for FormalParameter { ref name, .. } in &params {
                    params_varmap.insert(name.clone(), None);
                }

                self.push_new_function_level(params_varmap);

                let body = Box::new(self.visit(*body)?);
                let (params_varmap, _, fv) = self.pop_level().as_function_level();
                println!("fv {:?}", fv);

                for FormalParameter {
                    ref name,
                    ref mut bound,
                    ..
                } in &mut params
                {
                    if let Some(offset) = params_varmap.get(name).unwrap() {
                        *bound = Some(*offset);
                    }
                }

                let bound_variables = self.idgen.get_cur_id();
                self.idgen.restore();

                Ok(Node::new(
                    NodeBase::ArrowFunction {
                        params,
                        body,
                        bound_variables,
                    },
                    node.pos,
                ))
            }
            NodeBase::VarDecl(name, init, kind) => Ok(Node::new(
                NodeBase::VarDecl(
                    name,
                    if let Some(init) = init {
                        Some(Box::new(self.visit(*init)?))
                    } else {
                        None
                    },
                    kind,
                ),
                node.pos,
            )),
            NodeBase::Member(parent, property) => Ok(Node::new(
                NodeBase::Member(Box::new(self.visit(*parent)?), property),
                node.pos,
            )),
            NodeBase::Index(parent, index) => Ok(Node::new(
                NodeBase::Index(
                    Box::new(self.visit(*parent)?),
                    Box::new(self.visit(*index)?),
                ),
                node.pos,
            )),
            NodeBase::UnaryOp(expr, op) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.visit(*expr)?), op),
                node.pos,
            )),
            NodeBase::BinaryOp(lhs, rhs, op) => Ok(Node::new(
                NodeBase::BinaryOp(Box::new(self.visit(*lhs)?), Box::new(self.visit(*rhs)?), op),
                node.pos,
            )),
            NodeBase::Assign(dst, src) => Ok(Node::new(
                NodeBase::Assign(Box::new(self.visit(*dst)?), Box::new(self.visit(*src)?)),
                node.pos,
            )),
            NodeBase::Call(callee, args) => Ok(Node::new(
                NodeBase::Call(Box::new(self.visit(*callee)?), {
                    let mut new_args = vec![];
                    for arg in args {
                        new_args.push(self.visit(arg)?)
                    }
                    new_args
                }),
                node.pos,
            )),
            NodeBase::Throw(val) => Ok(Node::new(
                NodeBase::Throw(Box::new(self.visit(*val)?)),
                node.pos,
            )),
            NodeBase::Return(val) => Ok(Node::new(
                NodeBase::Return(if let Some(val) = val {
                    Some(Box::new(self.visit(*val)?))
                } else {
                    None
                }),
                node.pos,
            )),
            NodeBase::New(expr) => Ok(Node::new(
                NodeBase::New(Box::new(self.visit(*expr)?)),
                node.pos,
            )),
            NodeBase::Object(properties) => Ok(Node::new(
                NodeBase::Object({
                    let mut new_properties = vec![];
                    for property in properties {
                        match property {
                            PropertyDefinition::IdentifierReference(name) => {
                                new_properties.push(PropertyDefinition::IdentifierReference(name))
                            }
                            PropertyDefinition::Property(name, val) => new_properties
                                .push(PropertyDefinition::Property(name, self.visit(val)?)),
                            PropertyDefinition::MethodDefinition(kind, name, body) => {
                                new_properties.push(PropertyDefinition::MethodDefinition(
                                    kind,
                                    name,
                                    self.visit(body)?,
                                ))
                            }
                        }
                    }
                    new_properties
                }),
                node.pos,
            )),
            NodeBase::Array(elems) => Ok(Node::new(
                NodeBase::Array({
                    let mut new_elems = vec![];
                    for elem in elems {
                        new_elems.push(self.visit(elem)?)
                    }
                    new_elems
                }),
                node.pos,
            )),
            NodeBase::Identifier(info) => {
                let name = info.get_name();
                Ok(Node::new(
                    NodeBase::Identifier(if let Some(offset) = self.use_variable(&name) {
                        IdentifierInfo::Offset(offset)
                    } else {
                        IdentifierInfo::Name(name)
                    }),
                    node.pos,
                ))
            }
            _ => Ok(node),
        }
    }
}

impl Analyzer {
    pub fn push_new_block_level(&mut self) {
        self.level.push(Level::Block {
            varmap: VarMap::default(),
        })
    }

    pub fn push_new_function_level(&mut self, params: VarMap) {
        self.level.push(Level::Function {
            params,
            varmap: VarMap::default(),
            fv: FVSet::default(),
        })
    }

    pub fn pop_level(&mut self) -> Level {
        self.level.pop().unwrap()
    }

    pub fn in_function_level(&self) -> bool {
        for level in self.level.iter().rev() {
            match level {
                Level::Function { .. } => return true,
                _ => {}
            }
        }
        false
    }

    pub fn get_current_varmap_mut(&mut self) -> &mut VarMap {
        match self.level.last_mut().unwrap() {
            Level::Function { ref mut varmap, .. } => varmap,
            Level::Block { ref mut varmap } => varmap,
        }
    }

    pub fn get_current_varmap(&self) -> &VarMap {
        match self.level.last().unwrap() {
            Level::Function { ref varmap, .. } => varmap,
            Level::Block { ref varmap } => varmap,
        }
    }

    pub fn use_variable(&mut self, name: &String) -> Option<usize> {
        for level in self.level.iter_mut().rev() {
            match level {
                Level::Function {
                    ref mut varmap,
                    ref mut params,
                    ref mut fv,
                } => {
                    if let Some(v) = params.get_mut(name) {
                        if v.is_some() {
                            return *v;
                        }
                        *v = Some(self.idgen.gen_id());
                        return *v;
                    }

                    if let Some(v) = varmap.get_mut(name) {
                        if v.is_some() {
                            return *v;
                        }
                        *v = Some(self.idgen.gen_id());
                        return *v;
                    }

                    fv.insert(name.clone());

                    return None;
                }
                Level::Block { ref mut varmap } => {
                    if let Some(v) = varmap.get_mut(name) {
                        if v.is_some() {
                            return *v;
                        }
                        *v = Some(self.idgen.gen_id());
                        return *v;
                    }
                }
            }
        }

        None
    }
}

impl Level {
    pub fn get_varmap(&self) -> &VarMap {
        match self {
            Level::Function { ref varmap, .. } => varmap,
            Level::Block { ref varmap } => varmap,
        }
    }

    pub fn as_function_level(self) -> (VarMap, VarMap, FVSet) {
        match self {
            Level::Function { params, varmap, fv } => (params, varmap, fv),
            _ => panic!(),
        }
    }
}
