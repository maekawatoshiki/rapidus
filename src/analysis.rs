use bytecode_gen::{ByteCode, ByteCodeGenerator, VMInst};
use gc::MemoryAllocator;
use node::{
    BinOp, FormalParameter, FormalParameters, MethodDefinitionKind, Node, NodeBase,
    PropertyDefinition, UnaryOp, VarKind,
};
use parser::Error;
use vm::constant::{ConstantTable, SpecialProperties, SpecialPropertyKind};
use vm::jsvalue::function::{DestinationKind, Exception};
use vm::jsvalue::value::Value2;
use vm::jsvalue::{prototype, value};

#[derive(Debug)]
pub struct Analyzer {}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {}
    }

    pub fn analyze(&mut self, node: Node) -> Result<Node, Error> {
        self.visit(node)
    }
}

impl Analyzer {
    fn visit(&mut self, node: Node) -> Result<Node, Error> {
        match node.base {
            NodeBase::StatementList(_) => Ok(node),
            // NodeBase::If(ref cond, ref then, ref else_) => {
            //     self.visit_if(&*cond, &*then, &*else_, iseq)?
            // }
            _ => Ok(node)
            // NodeBase::Block(ref node_list) => {
            //     self.visit_block_statement(node_list, iseq, use_value)?
            // }
            // NodeBase::While(ref cond, ref body) => self.visit_while(&*cond, &*body, iseq)?,
            // NodeBase::For(ref init, ref cond, ref step, ref body) => {
            //     self.visit_for(&*init, &*cond, &*step, &*body, iseq)?
            // }
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
        }
    }
}
