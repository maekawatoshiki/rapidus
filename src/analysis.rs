use bytecode_gen::{ByteCode, ByteCodeGenerator, VMInst};
use gc::MemoryAllocator;
use id::IdGen;
use node::{
    BinOp, FormalParameter, FormalParameters, MethodDefinitionKind, Node, NodeBase,
    PropertyDefinition, UnaryOp, VarKind,
};
use parser::Error;
use rustc_hash::FxHashMap;
use vm::constant::{ConstantTable, SpecialProperties, SpecialPropertyKind};
use vm::jsvalue::function::{DestinationKind, Exception};
use vm::jsvalue::value::Value2;
use vm::jsvalue::{prototype, value};

#[derive(Debug)]
pub struct Analyzer {}

pub type VarMap = FxHashMap<String, usize>;

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {}
    }

    pub fn analyze(&mut self, node: Node) -> Result<Node, Error> {
        let mut var_map = FxHashMap::default();
        let mut id_gen = IdGen::new();

        self.visit(node, &mut var_map, &mut id_gen)
    }
}

impl Analyzer {
    fn visit(
        &mut self,
        node: Node,
        var_map: &mut VarMap,
        id_gen: &mut IdGen,
    ) -> Result<Node, Error> {
        match node.base {
            NodeBase::StatementList(stmts) => {
                for stmt in &stmts {
                    match stmt.base {
                        NodeBase::VarDecl(ref name, _, _) => {
                            var_map.insert(name.clone(), id_gen.gen_id());
                        }
                        _ => {}
                    }
                }

                let mut new_stmts = vec![];

                for stmt in stmts {
                    new_stmts.push(self.visit(stmt, var_map, id_gen)?)
                }

                Ok(Node::new(NodeBase::StatementList(new_stmts), node.pos))
            }
            NodeBase::Block(stmts) => {
                for stmt in &stmts {
                    match stmt.base {
                        NodeBase::VarDecl(ref name, _, _) => {
                            var_map.insert(name.clone(), id_gen.gen_id());
                        }
                        _ => {}
                    }
                }

                let mut new_stmts = vec![];

                for stmt in stmts {
                    new_stmts.push(self.visit(stmt, var_map, id_gen)?)
                }

                Ok(Node::new(NodeBase::Block(new_stmts), node.pos))
            }

            NodeBase::If(cond, then, else_) => Ok(Node::new(
                NodeBase::If(
                    Box::new(self.visit(*cond, var_map, id_gen)?),
                    Box::new(self.visit(*then, var_map, id_gen)?),
                    Box::new(self.visit(*else_, var_map, id_gen)?),
                ),
                node.pos,
            )),
            NodeBase::While(cond, body) => Ok(Node::new(
                NodeBase::While(
                    Box::new(self.visit(*cond, var_map, id_gen)?),
                    Box::new(self.visit(*body, var_map, id_gen)?),
                ),
                node.pos,
            )),
            NodeBase::For(init, cond, step, body) => Ok(Node::new(
                NodeBase::For(
                    Box::new(self.visit(*init, var_map, id_gen)?),
                    Box::new(self.visit(*cond, var_map, id_gen)?),
                    Box::new(self.visit(*step, var_map, id_gen)?),
                    Box::new(self.visit(*body, var_map, id_gen)?),
                ),
                node.pos,
            )),
            // NodeBase::Break(ref name) => self.visit_break(name, iseq)?,
            // NodeBase::Try(ref try, ref catch, ref param, ref finally) => {
            //     self.visit_try(&*try, &*catch, &*param, &*finally, iseq)?
            // }
            // NodeBase::FunctionDecl(ref name, ref params, ref body) => {
            //     self.visit_function_decl(name, params, &*body)?
            // }
            // NodeBase::FunctionExpr(ref name, ref params, ref body) => {
            //     self.visit_function_expr(name, params, &*body, true, iseq, use_value)?
            // }
            // NodeBase::ArrowFunction(ref params, ref body) => {
            //     self.visit_function_expr(&None, params, &*body, false, iseq, use_value)?
            // }
            // NodeBase::VarDecl(ref name, ref init, ref kind) => {
            //     self.visit_var_decl(node, name, init, kind, iseq)?
            // }
            // NodeBase::Member(ref parent, ref property) => {
            //     self.visit_member(&*parent, property, iseq, use_value)?
            // }
            // NodeBase::Index(ref parent, ref index) => {
            //     self.visit_index(&*parent, &*index, iseq, use_value)?
            // }
            // NodeBase::UnaryOp(ref expr, ref op) => {
            //     self.visit_unary_op(&*expr, op, iseq, use_value)?
            // }
            // NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
            //     self.visit_binary_op(&*lhs, &*rhs, op, iseq, use_value)?
            // }
            // NodeBase::Assign(ref dst, ref src) => {
            //     self.visit_assign(&*dst, &*src, iseq, use_value)?
            // }
            // NodeBase::Call(ref callee, ref args) => {
            //     self.visit_call(&*callee, args, iseq, use_value)?
            // }
            // NodeBase::Throw(ref val) => self.visit_throw(val, iseq)?,
            // NodeBase::Return(ref val) => self.visit_return(val, iseq)?,
            // NodeBase::New(ref expr) => self.visit_new(&*expr, iseq, use_value)?,
            // NodeBase::Object(ref properties) => self.visit_object_literal(properties, iseq)?,
            // NodeBase::Array(ref elems) => self.visit_array_literal(elems, iseq)?,
            // NodeBase::Identifier(ref name) => {
            //     if use_value {
            //         self.bytecode_generator.append_get_value(name, iseq)
            //     }
            // }
            // // NodeBase::Undefined => {
            // //     if use_value {
            // //         self.bytecode_generator.append_push_undefined(iseq);
            // //     }
            // // }
            // NodeBase::Null => {
            //     if use_value {
            //         self.bytecode_generator.append_push_null(iseq);
            //     }
            // }
            // NodeBase::This => {
            //     if use_value {
            //         self.bytecode_generator.append_push_this(iseq);
            //     }
            // }
            // NodeBase::String(ref s) => {
            //     if use_value {
            //         self.bytecode_generator
            //             .append_push_const(Value2::string(self.memory_allocator, s.clone()), iseq)
            //     }
            // }
            // NodeBase::Number(n) => {
            //     if use_value {
            //         self.bytecode_generator.append_push_number(n, iseq)
            //     }
            // }
            // NodeBase::Boolean(b) => {
            //     if use_value {
            //         self.bytecode_generator.append_push_bool(b, iseq)
            //     }
            // }
            // NodeBase::Nope => {
            //     if use_value {
            //         self.bytecode_generator
            //             .append_push_const(Value2::empty(), iseq)
            //     }
            // }
            // ref e => unimplemented!("{:?}", e),
            _ => Ok(node),
        }
    }
}
