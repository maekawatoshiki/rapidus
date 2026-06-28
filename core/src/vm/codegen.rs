use crate::builtins;
use crate::bytecode_gen::{ByteCode, ByteCodeGenerator, VMInst};
use crate::vm::constant::{ConstantTable, SpecialProperties, SpecialPropertyKind};
use crate::vm::factory::FunctionId;
use crate::vm::jsvalue::function::{
    DestinationKind, Exception, FuncInfoRef, ThisMode, UserFunctionInfo,
};
use crate::vm::jsvalue::value;
use crate::vm::vm::Factory;
use rapidus_ast::{
    loc::SourceLoc, ArrayPatternElement, BinOp, FormalParameter, FormalParameters,
    MethodDefinitionKind, Node, NodeBase, ObjectPatternProperty, PropertyDefinition, UnaryOp,
    VarKind,
};
use rustc_hash::FxHashMap;

pub type CodeGenResult = Result<(), Error>;

#[derive(Clone, Debug)]
pub struct Error {
    pub msg: String,
    pub loc: SourceLoc,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    General,
    Unimplemented,
}

#[derive(Debug)]
pub struct CodeGenerator<'a> {
    pub bytecode_generator: ByteCodeGenerator<'a>,
    pub factory: &'a mut Factory,
    pub function_stack: Vec<FunctionInfo>,
    pub pending_labels: Vec<String>,
    pub to_source_map: FxHashMap<FunctionId, ToSourcePos>,
    pub loc: SourceLoc,
    pub module_func_id: FunctionId,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: Option<String>,
    pub strict: bool,
    pub param_names: Vec<String>,
    pub var_names: Vec<String>,
    pub lex_names: Vec<String>,
    pub const_names: Vec<String>,
    pub func_decls: Vec<FuncInfoRef>,
    pub level: Vec<Level>,
    pub exception_table: Vec<Exception>,
    pub to_source_pos: ToSourcePos,
    pub module_func_id: FunctionId,
}

#[derive(Debug, Clone)]
/// Table of correspondence of an instruction pointer and char position on script.
pub struct ToSourcePos {
    table: Vec<(usize, SourceLoc)>,
    module_func_id: FunctionId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Level {
    Function,
    Block {
        names: Vec<String>,
    },
    TryOrCatch {
        finally_jmp_instr_pos: Vec<usize>,
    },
    Finally,
    Loop {
        labels: Vec<String>,
        break_jmp_instr_pos: Vec<usize>,
        continue_jmp_instr_pos: Vec<usize>,
    },
    Switch {
        labels: Vec<String>,
        break_jmp_instr_pos: Vec<usize>,
    },
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        constant_table: &'a mut ConstantTable,
        factory: &'a mut Factory,
        module_func_id: FunctionId,
    ) -> Self {
        CodeGenerator {
            bytecode_generator: ByteCodeGenerator::new(constant_table),
            factory,
            function_stack: vec![FunctionInfo::new(None, module_func_id) /* = global */],
            pending_labels: vec![],
            to_source_map: FxHashMap::default(),
            loc: SourceLoc::default(),
            module_func_id,
        }
    }

    pub fn compile(&mut self, node: &Node, use_value: bool) -> Result<FuncInfoRef, Error> {
        let mut iseq = vec![];
        self.current_function().strict = is_strict_body(node);
        self.visit(node, &mut iseq, use_value)?;
        self.bytecode_generator.append_return(&mut iseq);

        let function_info = self.function_stack.pop().unwrap();
        let strict = function_info.strict;
        let module_id = self.module_func_id;
        self.to_source_map
            .insert(module_id, function_info.to_source_pos.clone());

        let user_func_info = UserFunctionInfo {
            func_name: None,
            func_id: module_id,
            module_func_id: module_id,
            params: vec![],
            length: 0,
            var_names: function_info.var_names,
            lex_names: function_info.lex_names,
            const_names: function_info.const_names,
            func_decls: function_info.func_decls,
            constructible: false,
            generator: false,
            async_function: false,
            this_mode: if strict {
                ThisMode::Strict
            } else {
                ThisMode::Global
            },
            code: iseq,
            parameter_init_len: 0,
            exception_table: function_info.exception_table,
        };

        Ok(self.factory.alloc_user_func_info(module_id, user_func_info))
    }
}

// Visit methods for each Node

impl<'a> CodeGenerator<'a> {
    fn visit(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        self.loc = node.loc;
        match node.base {
            NodeBase::StatementList(ref node_list) => {
                self.visit_statement_list(node_list, iseq, use_value)?
            }
            NodeBase::Block(ref node_list) => {
                self.visit_block_statement(node_list, iseq, use_value)?
            }
            NodeBase::If(ref cond, ref then, ref else_) => {
                self.visit_if(&*cond, &*then, &*else_, iseq)?
            }
            NodeBase::While(ref cond, ref body) => self.visit_while(&*cond, &*body, iseq)?,
            NodeBase::DoWhile(ref body, ref cond) => self.visit_do_while(&*body, &*cond, iseq)?,
            NodeBase::With(ref object, ref body) => self.visit_with(&*object, &*body, iseq)?,
            NodeBase::For(ref init, ref cond, ref step, ref body) => {
                self.visit_for(&*init, &*cond, &*step, &*body, iseq)?
            }
            NodeBase::ForIn(ref left, ref right, ref body) => {
                self.visit_for_in(&*left, &*right, &*body, iseq)?
            }
            NodeBase::ForOf(ref left, ref right, ref body) => {
                self.visit_for_of(&*left, &*right, &*body, iseq)?
            }
            NodeBase::Switch(ref val, ref block) => self.visit_switch(&*val, &*block, iseq)?,
            NodeBase::CaseLabel(_) | NodeBase::DefaultLabel => {}
            NodeBase::Label(ref name, ref body) => {
                self.visit_label(name, &*body, iseq, use_value)?
            }
            NodeBase::Break(ref name) => self.visit_break(name, iseq)?,
            NodeBase::Continue(ref name) => self.visit_continue(name, iseq)?,
            NodeBase::Try(ref try_clause, ref catch, ref param, ref finally) => {
                self.visit_try(&*try_clause, &*catch, &*param, &*finally, iseq)?
            }
            NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.visit_function_decl(name, params, &*body, false, iseq)?
            }
            NodeBase::DerivedConstructorDecl(ref name, ref params, ref body) => {
                self.visit_function_decl(name, params, &*body, true, iseq)?
            }
            NodeBase::ClassHeritageSetup(ref name, ref heritage) => {
                self.visit_class_heritage_setup(name, &*heritage, iseq)?
            }
            NodeBase::AsyncFunctionDecl(ref name, ref params, ref body) => {
                self.visit_async_function_decl(name, params, &*body, iseq)?
            }
            NodeBase::GeneratorFunctionDecl(ref name, ref params, ref body) => {
                self.visit_generator_function_decl(name, params, &*body, iseq)?
            }
            NodeBase::AsyncGeneratorFunctionDecl(ref name, ref params, ref body) => {
                self.visit_async_generator_function_decl(name, params, &*body, iseq)?
            }
            NodeBase::FunctionExpr(ref name, ref params, ref body) => {
                self.visit_function_expr(name, params, &*body, false, iseq, use_value)?
            }
            NodeBase::AsyncFunctionExpr(ref name, ref params, ref body) => {
                self.visit_async_function_expr(name, params, &*body, iseq, use_value)?
            }
            NodeBase::GeneratorFunctionExpr(ref name, ref params, ref body) => {
                self.visit_generator_function_expr(name, params, &*body, iseq, use_value)?
            }
            NodeBase::AsyncGeneratorFunctionExpr(ref name, ref params, ref body) => {
                self.visit_async_generator_function_expr(name, params, &*body, iseq, use_value)?
            }
            NodeBase::ArrowFunction(ref params, ref body) => {
                self.visit_function_expr(&None, params, &*body, true, iseq, use_value)?
            }
            NodeBase::AsyncArrowFunction(ref params, ref body) => {
                self.visit_async_arrow_function_expr(params, &*body, iseq, use_value)?
            }
            NodeBase::AnonymousClassExpr(ref expr) => {
                self.visit(&*expr, iseq, use_value)?;
                if use_value {
                    self.bytecode_generator
                        .append_set_function_name(&"".to_string(), iseq);
                }
            }
            NodeBase::VarDecl(ref name, ref init, ref kind) => {
                self.visit_var_decl(node, name, init, kind, iseq)?
            }
            NodeBase::VarDeclPattern(ref pattern, ref init, ref kind) => {
                self.visit_var_decl_pattern(node, &*pattern, init, kind, iseq)?
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit_member(&*parent, property, iseq, use_value)?
            }
            NodeBase::PrivateMember(ref parent, ref property) => {
                self.visit_private_member(&*parent, property, iseq, use_value)?
            }
            NodeBase::PrivateMemberInit(ref parent, ref property, _) => {
                self.visit_private_member(&*parent, property, iseq, use_value)?
            }
            NodeBase::PrivateAccessorInit(_, _, _) => {
                return Err(Error::new_general_error(
                    "Syntax error: invalid private accessor position.".to_string(),
                    node.loc,
                ))
            }
            NodeBase::Index(ref parent, ref index) => {
                self.visit_index(&*parent, &*index, iseq, use_value)?
            }
            NodeBase::UnaryOp(ref expr, ref op) => {
                self.visit_unary_op(&*expr, op, iseq, use_value)?
            }
            NodeBase::BinaryOp(ref lhs, ref rhs, ref op) => {
                self.visit_binary_op(&*lhs, &*rhs, op, iseq, use_value)?
            }
            NodeBase::Assign(ref dst, ref src) => {
                self.visit_assign(&*dst, &*src, iseq, use_value)?
            }
            NodeBase::AssignOp(ref dst, ref src, ref op) => {
                self.visit_assign_op(&*dst, &*src, op, iseq, use_value)?
            }
            NodeBase::Call(ref callee, ref args) => {
                self.visit_call(&*callee, args, iseq, use_value)?
            }
            NodeBase::SuperCall(ref args) => self.visit_super_call(args, iseq, use_value)?,
            NodeBase::SuperCallFromArguments => {
                self.visit_super_call_from_arguments(iseq, use_value)?
            }
            NodeBase::Throw(ref val) => self.visit_throw(val, iseq)?,
            NodeBase::Yield(ref val, is_yield_star) => {
                self.visit_yield(val, is_yield_star, iseq, use_value)?
            }
            NodeBase::Await(ref val) => self.visit(val, iseq, use_value)?,
            NodeBase::Return(ref val) => self.visit_return(val, iseq)?,
            NodeBase::New(ref expr) => self.visit_new(&*expr, iseq, use_value)?,
            NodeBase::Object(ref properties) => {
                self.visit_object_literal(properties, iseq)?;
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
            }
            NodeBase::Array(ref elems) => {
                self.visit_array_literal(elems, iseq)?;
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
            }
            NodeBase::ArrayPattern(_) | NodeBase::ObjectPattern(_) => {
                return Err(Error::new_general_error(
                    "Syntax error: invalid destructuring pattern position.".to_string(),
                    node.loc,
                ))
            }
            NodeBase::Identifier(ref name) => {
                self.save_source_pos(iseq);
                self.bytecode_generator.append_get_value(name, iseq);
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
            }
            NodeBase::Spread(ref node) => {
                self.visit(node, iseq, true)?;
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                } else {
                    self.bytecode_generator.append_spread_array(iseq);
                }
            }
            NodeBase::Null => {
                if use_value {
                    self.bytecode_generator.append_push_null(iseq);
                }
            }
            NodeBase::This => {
                if use_value {
                    self.bytecode_generator.append_push_this(iseq);
                }
            }
            NodeBase::String(ref s) => {
                if use_value {
                    self.bytecode_generator
                        .append_push_const(self.factory.string(s.clone()), iseq)
                }
            }
            NodeBase::Number(n) => {
                if use_value {
                    self.bytecode_generator.append_push_number(n, iseq)
                }
            }
            NodeBase::BigInt(ref n) => {
                if use_value {
                    self.bytecode_generator
                        .append_push_const(self.factory.bigint(n.clone()), iseq)
                }
            }
            NodeBase::Boolean(b) => {
                if use_value {
                    self.bytecode_generator.append_push_bool(b, iseq)
                }
            }
            NodeBase::Nope => {
                if use_value {
                    self.bytecode_generator.append_push_undefined(iseq)
                }
            }
            NodeBase::TernaryOp(ref condition, ref then_clause, ref else_clause) => {
                self.visit_ternary_op(&*condition, &*then_clause, &*else_clause, iseq, use_value)?
            }
        }

        Ok(())
    }

    fn visit_statement_list(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        for node in node_list {
            self.visit(node, iseq, use_value)?;
        }

        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        node_list: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        let id = self
            .bytecode_generator
            .constant_table
            .add_lex_env_info(vec![]);
        self.bytecode_generator.append_push_env(id as u32, iseq);

        self.current_function().level.push(Level::new_block_level());

        for node in node_list {
            self.visit(node, iseq, use_value)?;
        }

        match self.current_function().level.pop().unwrap() {
            Level::Block { names } => {
                *self
                    .bytecode_generator
                    .constant_table
                    .get_mut(id)
                    .as_lex_env_info_mut() = names;
            }
            _ => unreachable!(),
        };

        self.bytecode_generator.append_pop_env(iseq);

        Ok(())
    }

    fn visit_if(
        &mut self,
        cond: &Node,
        then: &Node,
        else_: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp_if_false(0, iseq);

        self.visit(then, iseq, false)?;

        if else_.base == NodeBase::Nope {
            let pos = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
            );
        } else {
            let then_end_pos = iseq.len() as isize;
            self.bytecode_generator.append_jmp(0, iseq);

            let pos = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (pos - cond_pos) as i32 - 5,
                &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
            );

            self.visit(else_, iseq, false)?;

            let pos = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (pos - then_end_pos) as i32 - 5,
                &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
            );
        }

        Ok(())
    }

    pub fn visit_while(&mut self, cond: &Node, body: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        // name:
        //   while(...) {} // <- this while is named 'name'
        // let name = self.state.loop_names.pop();
        let labels = self.consume_pending_labels();
        self.current_function().level.push(Level::Loop {
            labels,
            break_jmp_instr_pos: vec![],
            continue_jmp_instr_pos: vec![],
        });

        let start = iseq.len() as isize;

        // self.bytecode_generator.append_loop_start(iseq);

        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp_if_false(0, iseq);

        self.visit(body, iseq, false)?;

        let loop_pos = iseq.len() as isize;
        self.bytecode_generator
            .append_jmp((start - loop_pos) as i32 - 5, iseq);

        let end = iseq.len() as isize;
        self.bytecode_generator.replace_int32(
            (end - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.current_function()
            .level
            .pop()
            .unwrap()
            .replace_break_and_continue(&mut self.bytecode_generator, iseq, end, start);

        Ok(())
    }

    pub fn visit_do_while(
        &mut self,
        body: &Node,
        cond: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let labels = self.consume_pending_labels();
        self.current_function().level.push(Level::Loop {
            labels,
            break_jmp_instr_pos: vec![],
            continue_jmp_instr_pos: vec![],
        });

        let start = iseq.len() as isize;

        self.visit(body, iseq, false)?;

        let cond_start = iseq.len() as isize;
        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator
            .append_jmp_if_true((start - cond_pos) as i32 - 5, iseq);

        let end = iseq.len() as isize;

        self.current_function()
            .level
            .pop()
            .unwrap()
            .replace_break_and_continue(&mut self.bytecode_generator, iseq, end, cond_start);

        Ok(())
    }

    pub fn visit_with(&mut self, object: &Node, body: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        if self.current_function().strict {
            return Err(Error::new_general_error(
                "Syntax error: strict mode code may not include a with statement.".to_string(),
                object.loc,
            ));
        }

        self.visit(object, iseq, true)?;
        self.bytecode_generator.append_push_object_env(iseq);
        self.current_function()
            .level
            .push(Level::Block { names: vec![] });
        let result = self.visit(body, iseq, false);
        self.current_function().level.pop().unwrap().as_block();
        self.bytecode_generator.append_pop_env(iseq);
        result
    }

    pub fn visit_for(
        &mut self,
        init: &Node,
        cond: &Node,
        step: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let labels = self.consume_pending_labels();
        self.current_function().level.push(Level::Loop {
            labels,
            break_jmp_instr_pos: vec![],
            continue_jmp_instr_pos: vec![],
        });

        self.visit(init, iseq, false)?;

        let start = iseq.len() as isize;

        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp_if_false(0, iseq);

        self.visit(body, iseq, false)?;

        let continue_pos = iseq.len() as isize;

        self.visit(step, iseq, false)?;

        let loop_pos = iseq.len() as isize;
        self.bytecode_generator
            .append_jmp((start - loop_pos) as i32 - 5, iseq);

        let end = iseq.len() as isize;
        self.bytecode_generator.replace_int32(
            (end - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.current_function()
            .level
            .pop()
            .unwrap()
            .replace_break_and_continue(&mut self.bytecode_generator, iseq, end, continue_pos);

        Ok(())
    }

    pub fn visit_for_in(
        &mut self,
        left: &Node,
        right: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let (name, per_iteration_lex_env) = match left.base {
            NodeBase::VarDecl(ref name, None, ref kind) => {
                match kind {
                    VarKind::Var => self.current_function().var_names.push(name.clone()),
                    VarKind::Let | VarKind::Const => {}
                }
                (name.clone(), *kind != VarKind::Var)
            }
            NodeBase::Identifier(ref name) => (name.clone(), false),
            _ => {
                return Err(Error::new_unimplemented_error(
                    "unsupported for-in left-hand side".to_string(),
                    left.loc,
                ))
            }
        };

        let labels = self.consume_pending_labels();
        self.current_function().level.push(Level::Loop {
            labels,
            break_jmp_instr_pos: vec![],
            continue_jmp_instr_pos: vec![],
        });

        self.visit(right, iseq, true)?;
        self.bytecode_generator.append_for_in_enumerate(iseq);

        let iteration_env_id = if per_iteration_lex_env {
            Some(
                self.bytecode_generator
                    .constant_table
                    .add_lex_env_info(vec![name.clone()]),
            )
        } else {
            None
        };

        let start = iseq.len() as isize;
        if let Some(env_id) = iteration_env_id {
            self.bytecode_generator.append_push_env(env_id as u32, iseq);
            self.current_function().level.push(Level::Block {
                names: vec![name.clone()],
            });
        }

        let next_pos = iseq.len() as isize;
        self.bytecode_generator.append_for_in_next(&name, 0, iseq);

        self.visit(body, iseq, false)?;

        if per_iteration_lex_env {
            self.current_function().level.pop().unwrap().as_block();
            self.bytecode_generator.append_pop_env(iseq);
        }

        let loop_pos = iseq.len() as isize;
        self.bytecode_generator
            .append_jmp((start - loop_pos) as i32 - 5, iseq);

        let iteration_cleanup = iseq.len() as isize;
        if per_iteration_lex_env {
            self.bytecode_generator.append_pop_env(iseq);
        }

        let cleanup = iseq.len() as isize;
        self.bytecode_generator.append_pop(iseq);
        let end = iseq.len() as isize;

        self.bytecode_generator.replace_int32(
            (iteration_cleanup - next_pos) as i32 - 9,
            &mut iseq[next_pos as usize + 5..next_pos as usize + 9],
        );

        self.current_function()
            .level
            .pop()
            .unwrap()
            .replace_break_and_continue(&mut self.bytecode_generator, iseq, cleanup, start);

        let _ = end;
        Ok(())
    }

    pub fn visit_for_of(
        &mut self,
        left: &Node,
        right: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let (name, pattern, pattern_is_binding, per_iteration_lex_env, iteration_names) =
            match left.base {
                NodeBase::VarDecl(ref name, None, ref kind) => {
                    match kind {
                        VarKind::Var => self.current_function().var_names.push(name.clone()),
                        VarKind::Let | VarKind::Const => {}
                    }
                    (
                        Some(name.clone()),
                        None,
                        true,
                        *kind != VarKind::Var,
                        vec![name.clone()],
                    )
                }
                NodeBase::VarDeclPattern(ref pattern, None, ref kind) => {
                    let names = Self::pattern_bound_names(pattern);
                    match kind {
                        VarKind::Var => {
                            for name in &names {
                                self.current_function().var_names.push(name.clone());
                            }
                        }
                        VarKind::Let | VarKind::Const => {}
                    }
                    (None, Some(&**pattern), true, *kind != VarKind::Var, names)
                }
                NodeBase::Identifier(ref name) => (Some(name.clone()), None, false, false, vec![]),
                NodeBase::ArrayPattern(_) | NodeBase::ObjectPattern(_) => {
                    (None, Some(left), false, false, vec![])
                }
                _ => {
                    return Err(Error::new_unimplemented_error(
                        "unsupported for-of left-hand side".to_string(),
                        left.loc,
                    ))
                }
            };

        let labels = self.consume_pending_labels();
        self.current_function().level.push(Level::Loop {
            labels,
            break_jmp_instr_pos: vec![],
            continue_jmp_instr_pos: vec![],
        });

        self.visit(right, iseq, true)?;
        self.bytecode_generator.append_for_of_enumerate(iseq);

        let iteration_env_id = if per_iteration_lex_env {
            Some(
                self.bytecode_generator
                    .constant_table
                    .add_lex_env_info(iteration_names.clone()),
            )
        } else {
            None
        };

        let start = iseq.len() as isize;
        if let Some(env_id) = iteration_env_id {
            self.bytecode_generator.append_push_env(env_id as u32, iseq);
            self.current_function().level.push(Level::Block {
                names: iteration_names.clone(),
            });
        }

        let next_pos = iseq.len() as isize;
        let next_instr_size = if let Some(name) = name {
            self.bytecode_generator.append_for_of_next(&name, 0, iseq);
            9
        } else {
            self.bytecode_generator.append_for_of_next_value(0, iseq);
            if let Some(pattern) = pattern {
                self.destructure_stack_top_to(pattern, iseq, pattern_is_binding)?;
            }
            5
        };

        self.visit(body, iseq, false)?;

        if per_iteration_lex_env {
            self.current_function().level.pop().unwrap().as_block();
            self.bytecode_generator.append_pop_env(iseq);
        }

        let loop_pos = iseq.len() as isize;
        self.bytecode_generator
            .append_jmp((start - loop_pos) as i32 - 5, iseq);

        let iteration_cleanup = iseq.len() as isize;
        if per_iteration_lex_env {
            self.bytecode_generator.append_pop_env(iseq);
        }

        let cleanup = iseq.len() as isize;
        self.bytecode_generator.append_pop(iseq);
        let end = iseq.len() as isize;

        if next_instr_size == 9 {
            self.bytecode_generator.replace_int32(
                (iteration_cleanup - next_pos) as i32 - 9,
                &mut iseq[next_pos as usize + 5..next_pos as usize + 9],
            );
        } else {
            self.bytecode_generator.replace_int32(
                (iteration_cleanup - next_pos) as i32 - 5,
                &mut iseq[next_pos as usize + 1..next_pos as usize + 5],
            );
        }

        self.current_function()
            .level
            .pop()
            .unwrap()
            .replace_break_and_continue(&mut self.bytecode_generator, iseq, cleanup, start);

        let _ = end;
        Ok(())
    }

    pub fn visit_switch(&mut self, val: &Node, block: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        let items = match block.base {
            NodeBase::Block(ref items) | NodeBase::StatementList(ref items) => items,
            _ => {
                return Err(Error::new_general_error(
                    "Syntax error: invalid switch body.".to_string(),
                    block.loc,
                ))
            }
        };

        let mut cases: Vec<(Option<&Node>, Vec<&Node>)> = vec![];
        let mut default_index = None;
        for item in items {
            match item.base {
                NodeBase::CaseLabel(ref case_val) => cases.push((Some(&**case_val), vec![])),
                NodeBase::DefaultLabel => {
                    default_index = Some(cases.len());
                    cases.push((None, vec![]));
                }
                _ => {
                    if let Some(case) = cases.last_mut() {
                        case.1.push(item);
                    }
                }
            }
        }

        let env_id = self
            .bytecode_generator
            .constant_table
            .add_lex_env_info(vec![]);
        self.bytecode_generator.append_push_env(env_id as u32, iseq);
        let labels = self.consume_pending_labels();
        self.current_function()
            .level
            .push(Level::new_switch_level(labels));
        self.current_function().level.push(Level::new_block_level());

        self.visit(val, iseq, true)?;

        if cases.is_empty() {
            self.bytecode_generator.append_pop(iseq);
        } else {
            let mut match_jmp_positions = vec![];
            for (case_index, (case_val, _)) in cases.iter().enumerate() {
                let Some(case_val) = case_val else {
                    continue;
                };
                self.bytecode_generator.append_double(iseq);
                self.visit(case_val, iseq, true)?;
                self.bytecode_generator.append_seq(iseq);
                let match_jmp_pos = iseq.len();
                self.bytecode_generator.append_jmp_if_true(0, iseq);
                match_jmp_positions.push((match_jmp_pos, case_index));
            }

            let no_match_jmp_pos = iseq.len();
            self.bytecode_generator.append_jmp(0, iseq);

            let mut entry_positions = vec![];
            let mut entry_jmp_positions = vec![];
            for _ in &cases {
                entry_positions.push(iseq.len());
                self.bytecode_generator.append_pop(iseq);
                let entry_jmp_pos = iseq.len();
                self.bytecode_generator.append_jmp(0, iseq);
                entry_jmp_positions.push(entry_jmp_pos);
            }

            let no_match_cleanup = if default_index.is_none() {
                let pos = iseq.len();
                self.bytecode_generator.append_pop(iseq);
                Some(pos)
            } else {
                None
            };
            let no_match_to_end = if default_index.is_none() {
                let pos = iseq.len();
                self.bytecode_generator.append_jmp(0, iseq);
                Some(pos)
            } else {
                None
            };

            for (i, (_, body)) in cases.iter().enumerate() {
                let body_start = iseq.len();
                self.bytecode_generator.replace_int32(
                    (body_start as isize - entry_jmp_positions[i] as isize) as i32 - 5,
                    &mut iseq[entry_jmp_positions[i] + 1..entry_jmp_positions[i] + 5],
                );
                for stmt in body {
                    self.visit(stmt, iseq, false)?;
                }
            }

            let body_end = iseq.len();
            let no_match_target = default_index
                .map(|index| entry_positions[index])
                .or(no_match_cleanup)
                .unwrap();
            self.bytecode_generator.replace_int32(
                (no_match_target as isize - no_match_jmp_pos as isize) as i32 - 5,
                &mut iseq[no_match_jmp_pos + 1..no_match_jmp_pos + 5],
            );
            if let Some(no_match_to_end) = no_match_to_end {
                self.bytecode_generator.replace_int32(
                    (body_end as isize - no_match_to_end as isize) as i32 - 5,
                    &mut iseq[no_match_to_end + 1..no_match_to_end + 5],
                );
            }
            for (match_pos, case_index) in match_jmp_positions {
                self.bytecode_generator.replace_int32(
                    (entry_positions[case_index] as isize - match_pos as isize) as i32 - 5,
                    &mut iseq[match_pos + 1..match_pos + 5],
                );
            }
        }

        match self.current_function().level.pop().unwrap() {
            Level::Block { names } => {
                *self
                    .bytecode_generator
                    .constant_table
                    .get_mut(env_id)
                    .as_lex_env_info_mut() = names;
            }
            _ => unreachable!(),
        };
        self.bytecode_generator.append_pop_env(iseq);
        let end = iseq.len() as isize;

        match self.current_function().level.pop().unwrap() {
            Level::Switch {
                break_jmp_instr_pos,
                ..
            } => {
                for instr_pos in break_jmp_instr_pos {
                    self.bytecode_generator.replace_int32(
                        (end - instr_pos as isize) as i32 - 5,
                        &mut iseq[instr_pos + 1..instr_pos + 5],
                    );
                }
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    pub fn visit_label(
        &mut self,
        name: &String,
        body: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        let len = self.pending_labels.len();
        self.pending_labels.push(name.clone());
        let result = self.visit(body, iseq, use_value);
        self.pending_labels.truncate(len);
        result
    }

    pub fn visit_break(&mut self, name: &Option<String>, iseq: &mut ByteCode) -> CodeGenResult {
        if !self.current_function().has_breakable_target(name) {
            return Err(Error::new_general_error(
                "Syntax error: Illegal break statement.".to_string(),
                self.loc,
            ));
        }
        self.unwind_breakable(name, iseq);

        let break_instr_pos = iseq.len();
        self.bytecode_generator.append_jmp(0, iseq);

        self.current_function()
            .get_last_breakable_target(name)
            .as_breakable_mut()
            .push(break_instr_pos);

        Ok(())
    }

    pub fn visit_continue(&mut self, name: &Option<String>, iseq: &mut ByteCode) -> CodeGenResult {
        if !self.current_function().has_loop_target(name) {
            return Err(Error::new_general_error(
                "Syntax error: Illegal continue statement.".to_string(),
                self.loc,
            ));
        }
        self.unwind_loop(name, iseq);

        let continue_instr_pos = iseq.len();
        self.bytecode_generator.append_jmp(0, iseq);

        self.current_function()
            .get_last_loop_target(name)
            .as_loop_mut()
            .1
            .push(continue_instr_pos);

        Ok(())
    }

    pub fn visit_try(
        &mut self,
        try_clause: &Node,
        catch: &Node,
        param: &Node,
        finally: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        // TODO: Refine code

        let has_catch = catch.base != NodeBase::Nope;

        // Try block
        let (try_, try_to_finally, leave_try) = {
            let try_start = iseq.len() as usize;

            self.current_function()
                .level
                .push(Level::new_try_or_catch_level());

            self.visit(try_clause, iseq, false)?;

            let try_ = self.current_function().level.pop().unwrap();

            let try_to_finally = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let leave_try = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let try_end = iseq.len() as usize;
            self.current_function().exception_table.push(Exception {
                start: try_start,
                end: try_end,
                dst_kind: if has_catch {
                    DestinationKind::Catch
                } else {
                    DestinationKind::Finally
                },
            });

            (try_, try_to_finally, leave_try)
        };

        // Catch block
        let (catch_, catch_to_finally, leave_catch) = if has_catch {
            let catch_start = iseq.len() as usize;
            let param_names = match param.base {
                NodeBase::Nope => vec![],
                NodeBase::Identifier(ref name) => vec![name.clone()],
                NodeBase::ArrayPattern(_) | NodeBase::ObjectPattern(_) => {
                    Self::pattern_bound_names(param)
                }
                _ => unimplemented!(),
            };

            self.current_function()
                .level
                .push(Level::new_try_or_catch_level());
            let env_id = self
                .bytecode_generator
                .constant_table
                .add_lex_env_info(vec![]);
            self.bytecode_generator.append_push_env(env_id as u32, iseq);
            self.current_function()
                .level
                .push(Level::Block { names: param_names });
            self.save_source_pos(iseq);
            match param.base {
                NodeBase::Nope => self.bytecode_generator.append_pop(iseq),
                NodeBase::Identifier(ref name) => {
                    self.bytecode_generator.append_set_value(name, iseq)
                }
                NodeBase::ArrayPattern(_) | NodeBase::ObjectPattern(_) => {
                    self.destructure_stack_top_to(param, iseq, true)?;
                }
                _ => unimplemented!(),
            }

            self.visit(catch, iseq, false)?;

            self.bytecode_generator.append_pop_env(iseq);

            let names = self.current_function().level.pop().unwrap().as_block();
            let catch_ = self.current_function().level.pop().unwrap();
            *self
                .bytecode_generator
                .constant_table
                .get_mut(env_id)
                .as_lex_env_info_mut() = names;

            let catch_to_finally = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let leave_catch = iseq.len() as usize;
            self.bytecode_generator.append_jmp_sub(0, iseq);

            let catch_end = iseq.len() as usize;
            self.current_function().exception_table.push(Exception {
                start: catch_start,
                end: catch_end,
                dst_kind: DestinationKind::Finally,
            });

            (catch_, catch_to_finally, leave_catch)
        } else {
            (Level::new_try_or_catch_level(), 0, 0)
        };

        // Finally block
        let finally_start = iseq.len() as usize;

        let has_return_try =
            try_.set_jmp_to_finally(finally_start, &mut self.bytecode_generator, iseq)
                || catch_.set_jmp_to_finally(finally_start, &mut self.bytecode_generator, iseq);

        self.current_function().level.push(Level::Finally);
        self.visit(finally, iseq, false)?;

        let finally_to_outer_finally_jmp_instr_pos = iseq.len();
        if has_return_try
            && self
                .current_function()
                .find_last_try_or_catch()
                .map(|try_or_catch| {
                    try_or_catch
                        .as_try_or_catch_mut()
                        .push(finally_to_outer_finally_jmp_instr_pos)
                })
                .is_some()
        {
            self.bytecode_generator.append_jmp(0, iseq);
        }

        assert_eq!(self.current_function().level.pop().unwrap(), Level::Finally);
        self.bytecode_generator.append_return_sub(iseq);

        let finally_end = iseq.len() as usize;

        self.bytecode_generator.replace_int32(
            (finally_start - try_to_finally) as i32 - 5,
            &mut iseq[try_to_finally + 1..try_to_finally + 5],
        );
        self.bytecode_generator.replace_int32(
            (finally_end - leave_try) as i32 - 5,
            &mut iseq[leave_try + 1..leave_try + 5],
        );

        if has_catch {
            self.bytecode_generator.replace_int32(
                (finally_start - catch_to_finally) as i32 - 5,
                &mut iseq[catch_to_finally + 1..catch_to_finally + 5],
            );
            self.bytecode_generator.replace_int32(
                (finally_end - leave_catch) as i32 - 5,
                &mut iseq[leave_catch + 1..leave_catch + 5],
            );
        }

        Ok(())
    }

    fn visit_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
        derived_constructor: bool,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let is_block_decl = self.current_function().get_last_block().is_some();
        let func_info = self.visit_function(
            Some(name.clone()),
            params,
            body,
            false,
            false,
            false,
            derived_constructor,
        )?;
        if is_block_decl {
            if self.current_function().strict {
                self.current_function()
                    .get_last_block()
                    .unwrap()
                    .as_block_mut()
                    .push(name.clone());
            } else {
                self.current_function().var_names.push(name.clone());
            }
            let val = self.factory.function(func_info, None);
            self.bytecode_generator.append_push_const(val, iseq);
            self.bytecode_generator.append_set_outer_env(iseq);
            self.save_source_pos(iseq);
            self.bytecode_generator.append_set_value(name, iseq);
        } else {
            self.current_function().var_names.push(name.clone());
            self.current_function().func_decls.push(func_info);
        }
        Ok(())
    }

    fn visit_class_heritage_setup(
        &mut self,
        name: &String,
        heritage: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.visit(heritage, iseq, true)?;
        self.save_source_pos(iseq);
        self.bytecode_generator.append_get_value(name, iseq);
        let setup = self
            .factory
            .builtin_function("", builtins::class_heritage_setup);
        self.bytecode_generator.append_push_const(setup, iseq);
        self.save_source_pos(iseq);
        self.bytecode_generator.append_call(2, iseq);
        self.bytecode_generator.append_pop(iseq);
        Ok(())
    }

    fn visit_generator_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
        _iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let func_info =
            self.visit_function(Some(name.clone()), params, body, false, true, false, false)?;
        self.current_function().var_names.push(name.clone());
        self.current_function().func_decls.push(func_info);
        Ok(())
    }

    fn visit_async_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
        _iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let func_info =
            self.visit_function(Some(name.clone()), params, body, false, false, true, false)?;
        self.current_function().var_names.push(name.clone());
        self.current_function().func_decls.push(func_info);
        Ok(())
    }

    fn visit_async_generator_function_decl(
        &mut self,
        name: &String,
        params: &FormalParameters,
        body: &Node,
        _iseq: &mut ByteCode,
    ) -> CodeGenResult {
        let func_info =
            self.visit_function(Some(name.clone()), params, body, false, true, true, false)?;
        self.current_function().var_names.push(name.clone());
        self.current_function().func_decls.push(func_info);
        Ok(())
    }

    fn visit_function_expr(
        &mut self,
        name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        arrow_function: bool,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        let func_info = self.visit_function(
            name.clone(),
            params,
            body,
            arrow_function,
            false,
            false,
            false,
        )?;
        let val = self.factory.function(func_info, None);
        self.bytecode_generator.append_push_const(val, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

        Ok(())
    }

    fn visit_async_function_expr(
        &mut self,
        name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        let func_info =
            self.visit_function(name.clone(), params, body, false, false, true, false)?;
        let val = self.factory.function(func_info, None);
        self.bytecode_generator.append_push_const(val, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

        Ok(())
    }

    fn visit_generator_function_expr(
        &mut self,
        name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        let func_info =
            self.visit_function(name.clone(), params, body, false, true, false, false)?;
        let val = self.factory.function(func_info, None);
        self.bytecode_generator.append_push_const(val, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

        Ok(())
    }

    fn visit_async_generator_function_expr(
        &mut self,
        name: &Option<String>,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        let func_info =
            self.visit_function(name.clone(), params, body, false, true, true, false)?;
        let val = self.factory.function(func_info, None);
        self.bytecode_generator.append_push_const(val, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

        Ok(())
    }

    fn visit_async_arrow_function_expr(
        &mut self,
        params: &FormalParameters,
        body: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if !use_value {
            return Ok(());
        }

        let func_info = self.visit_function(None, params, body, true, false, true, false)?;
        let val = self.factory.function(func_info, None);
        self.bytecode_generator.append_push_const(val, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

        Ok(())
    }

    fn visit_function(
        &mut self,
        name: Option<String>,
        params: &FormalParameters,
        body: &Node,
        arrow_function: bool,
        generator: bool,
        async_function: bool,
        derived_constructor: bool,
    ) -> Result<FuncInfoRef, Error> {
        let parent_strict = self.current_function().strict;
        let mut new_function = FunctionInfo::new(name, self.module_func_id);
        new_function.strict = parent_strict || is_strict_body(body);
        if new_function.strict {
            if let Some(ref name) = new_function.name {
                if is_strict_reserved_word(name) {
                    return Err(Error::new_general_error(
                        format!("Unexpected strict mode reserved word '{}'", name),
                        body.loc,
                    ));
                }
            }
            for param in params {
                if let Some(ref pattern) = param.pattern {
                    for name in Self::pattern_bound_names(pattern) {
                        if is_strict_reserved_word(&name) {
                            return Err(Error::new_general_error(
                                format!("Unexpected strict mode reserved word '{}'", name),
                                body.loc,
                            ));
                        }
                    }
                } else if is_strict_reserved_word(&param.name) {
                    return Err(Error::new_general_error(
                        format!("Unexpected strict mode reserved word '{}'", param.name),
                        body.loc,
                    ));
                }
            }
        }
        self.function_stack.push(new_function);

        let mut func_iseq = vec![];

        self.emit_parameter_initializers(params, &mut func_iseq)?;
        let parameter_init_len = func_iseq.len();
        self.visit(body, &mut func_iseq, false)?;

        self.bytecode_generator
            .append_push_undefined(&mut func_iseq);
        self.bytecode_generator.append_return(&mut func_iseq);

        let length = params
            .iter()
            .take_while(|param| !param.is_rest_param && param.init.is_none())
            .count();

        let params = params
            .iter()
            .map(
                |FormalParameter {
                     name,
                     init,
                     pattern,
                     is_rest_param,
                     ..
                 }| value::FunctionParameter {
                    name: name.clone(),
                    rest_param: *is_rest_param,
                    has_initializer: init.is_some() || pattern.is_some(),
                },
            )
            .collect();

        let function_info = self.function_stack.pop().unwrap();

        let func_id = self.factory.new_func_id();

        self.to_source_map
            .insert(func_id, function_info.to_source_pos);

        let strict = function_info.strict;
        let user_func_info = UserFunctionInfo {
            func_name: function_info.name,
            func_id,
            module_func_id: self.module_func_id,
            params,
            length,
            var_names: function_info.var_names,
            lex_names: function_info.lex_names,
            const_names: function_info.const_names,
            func_decls: function_info.func_decls,
            constructible: !arrow_function && !generator && !async_function,
            generator,
            async_function,
            this_mode: if arrow_function {
                ThisMode::Lexical
            } else if derived_constructor {
                ThisMode::Derived
            } else if strict {
                ThisMode::Strict
            } else {
                ThisMode::Global
            },
            code: func_iseq,
            parameter_init_len,
            exception_table: function_info.exception_table,
        };

        let func_ref = self.factory.alloc_user_func_info(func_id, user_func_info);

        Ok(func_ref)
    }

    fn emit_parameter_initializers(
        &mut self,
        params: &FormalParameters,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        for param in params {
            if param.init.is_some() {
                self.save_source_pos(iseq);
                self.bytecode_generator.append_get_value(&param.name, iseq);
                self.apply_default_initializer(&param.init, Some(&param.name), iseq)?;
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_value(&param.name, iseq);
            }
            if let Some(ref pattern) = param.pattern {
                self.declare_pattern_names(pattern, &VarKind::Var)?;
                self.save_source_pos(iseq);
                self.bytecode_generator.append_get_value(&param.name, iseq);
                self.destructure_stack_top_to(pattern, iseq, true)?;
            }
        }
        Ok(())
    }

    pub fn visit_var_decl(
        &mut self,
        node: &Node,
        name: &String,
        init: &Option<Box<Node>>,
        kind: &VarKind,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.declare_binding_name(node, name, kind)?;

        if let &Some(ref init) = init {
            self.visit(&*init, iseq, true)?;
            self.append_inferred_name_if_needed(init, name, iseq);
            self.save_source_pos(iseq);
            self.bytecode_generator.append_set_value(name, iseq);
            // is_initialized = true;
        }

        Ok(())
    }

    fn visit_var_decl_pattern(
        &mut self,
        node: &Node,
        pattern: &Node,
        init: &Option<Box<Node>>,
        kind: &VarKind,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.declare_pattern_names(pattern, kind)?;
        if let &Some(ref init) = init {
            self.visit(&*init, iseq, true)?;
        } else {
            self.bytecode_generator.append_push_undefined(iseq);
        }
        self.destructure_stack_top_to(pattern, iseq, true)?;
        let _ = node;
        Ok(())
    }

    fn declare_binding_name(
        &mut self,
        node: &Node,
        name: &String,
        kind: &VarKind,
    ) -> CodeGenResult {
        if self.current_function().strict && is_strict_reserved_word(name) {
            return Err(Error::new_general_error(
                format!("Unexpected strict mode reserved word '{}'", name),
                node.loc,
            ));
        }

        match kind {
            VarKind::Var => {
                self.current_function().var_names.push(name.clone());
            }
            VarKind::Let => self.declare_lexical_binding_name(node, name.clone())?,
            // TODO: Const needs double-assignment check
            VarKind::Const => {
                self.declare_lexical_binding_name(node, name.clone())?;
                self.current_function().const_names.push(name.clone());
            }
        }

        Ok(())
    }

    fn declare_lexical_binding_name(&mut self, node: &Node, name: String) -> CodeGenResult {
        fn check_duplicate(names: &mut Vec<String>, name: String, node: &Node) -> CodeGenResult {
            if names.iter().find(|declared| *declared == &name).is_some() {
                return Err(Error::new_general_error(
                    format!("Identifier '{}' has already been declared", name),
                    node.loc,
                ));
            }
            names.push(name);
            Ok(())
        }
        let cur_func = self.current_function();
        if let Some(ref mut block) = cur_func.get_last_block() {
            return check_duplicate(block.as_block_mut(), name, node);
        }
        check_duplicate(&mut cur_func.lex_names, name, node)
    }

    fn declare_pattern_names(&mut self, pattern: &Node, kind: &VarKind) -> CodeGenResult {
        for name in Self::pattern_bound_names(pattern) {
            let node = Node::new(NodeBase::Identifier(name.clone()), pattern.loc);
            self.declare_binding_name(&node, &name, kind)?;
        }
        Ok(())
    }

    fn pattern_bound_names(pattern: &Node) -> Vec<String> {
        let mut names = vec![];
        match pattern.base {
            NodeBase::Identifier(ref name) => names.push(name.clone()),
            NodeBase::ArrayPattern(ref elements) => {
                for element in elements {
                    match element {
                        ArrayPatternElement::Element(target, _)
                        | ArrayPatternElement::Rest(target) => {
                            names.extend(Self::pattern_bound_names(target));
                        }
                        ArrayPatternElement::Elision => {}
                    }
                }
            }
            NodeBase::ObjectPattern(ref properties) => {
                for property in properties {
                    match property {
                        ObjectPatternProperty::Property(_, target, _)
                        | ObjectPatternProperty::ComputedProperty(_, target, _)
                        | ObjectPatternProperty::Rest(target) => {
                            names.extend(Self::pattern_bound_names(target));
                        }
                    }
                }
            }
            _ => {}
        }
        names
    }

    fn visit_member(
        &mut self,
        parent: &Node,
        member: &String,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        self.visit(parent, iseq, true)?;
        let property = self.factory.string(member.clone());
        self.bytecode_generator.append_push_const(property, iseq);
        self.save_source_pos(iseq);
        self.bytecode_generator.append_get_member(iseq);

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_private_member(
        &mut self,
        parent: &Node,
        member: &String,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        self.visit(parent, iseq, true)?;
        let property = self.factory.string(member.clone());
        self.bytecode_generator.append_push_const(property, iseq);
        self.save_source_pos(iseq);
        self.bytecode_generator.append_get_private_member(iseq);

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_index(
        &mut self,
        parent: &Node,
        index: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        self.visit(parent, iseq, true)?;
        self.visit(index, iseq, true)?;
        self.save_source_pos(iseq);
        self.bytecode_generator.append_get_member(iseq);

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_unary_op(
        &mut self,
        expr: &Node,
        op: &UnaryOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if let NodeBase::Identifier(ref name) = expr.base {
            match op {
                UnaryOp::PrInc | UnaryOp::PrDec | UnaryOp::PoInc | UnaryOp::PoDec => {
                    self.bytecode_generator
                        .append_get_value_keep_ref(name, iseq);
                    match op {
                        UnaryOp::PrInc | UnaryOp::PrDec => {
                            self.bytecode_generator.append_push_int8(1, iseq);
                            if matches!(op, UnaryOp::PrInc) {
                                self.bytecode_generator.append_add(iseq);
                            } else {
                                self.bytecode_generator.append_sub(iseq);
                            }
                            if use_value {
                                self.bytecode_generator.append_double(iseq);
                            }
                            self.bytecode_generator
                                .append_set_value_keep_ref(name, iseq);
                        }
                        UnaryOp::PoInc | UnaryOp::PoDec => {
                            if use_value {
                                self.bytecode_generator.append_double(iseq);
                            }
                            self.bytecode_generator.append_push_int8(1, iseq);
                            if matches!(op, UnaryOp::PoInc) {
                                self.bytecode_generator.append_add(iseq);
                            } else {
                                self.bytecode_generator.append_sub(iseq);
                            }
                            self.bytecode_generator
                                .append_set_value_keep_ref(name, iseq);
                        }
                        _ => unreachable!(),
                    }
                    return Ok(());
                }
                _ => {}
            }
        }

        match op {
            &UnaryOp::Delete => return self.visit_delete(expr, iseq, use_value),
            &UnaryOp::Void => {
                self.visit(expr, iseq, true)?;
                self.bytecode_generator.append_pop(iseq);
                if use_value {
                    self.bytecode_generator.append_push_undefined(iseq);
                }
                return Ok(());
            }
            _ => {}
        }

        self.visit(expr, iseq, true)?;

        match op {
            &UnaryOp::Typeof => self.bytecode_generator.append_typeof(iseq),
            &UnaryOp::Plus => self.bytecode_generator.append_posi(iseq),
            &UnaryOp::Minus => self.bytecode_generator.append_neg(iseq),
            &UnaryOp::Not => self.bytecode_generator.append_lnot(iseq),
            &UnaryOp::BitwiseNot => self.bytecode_generator.append_not(iseq),
            &UnaryOp::PrInc => {
                self.bytecode_generator.append_push_int8(1, iseq);
                self.bytecode_generator.append_add(iseq);
                if use_value {
                    self.bytecode_generator.append_double(iseq);
                }
                self.assign_stack_top_to(expr, iseq)?;
            }
            &UnaryOp::PrDec => {
                self.bytecode_generator.append_push_int8(1, iseq);
                self.bytecode_generator.append_sub(iseq);
                if use_value {
                    self.bytecode_generator.append_double(iseq);
                }
                self.assign_stack_top_to(expr, iseq)?;
            }
            &UnaryOp::PoInc => {
                self.bytecode_generator.append_double(iseq);
                self.bytecode_generator.append_push_int8(1, iseq);
                self.bytecode_generator.append_add(iseq);
                self.assign_stack_top_to(expr, iseq)?;
            }
            &UnaryOp::PoDec => {
                self.bytecode_generator.append_double(iseq);
                self.bytecode_generator.append_push_int8(1, iseq);
                self.bytecode_generator.append_sub(iseq);
                self.assign_stack_top_to(expr, iseq)?;
            }
            _ => unimplemented!(),
        }

        let leaves_value = !matches!(op, &UnaryOp::PrInc | &UnaryOp::PrDec) || use_value;
        if !use_value && leaves_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_delete(&mut self, expr: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        match expr.base {
            NodeBase::Member(ref parent, ref property) => {
                self.visit(&*parent, iseq, true)?;
                let property = self.factory.string(property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                if self.current_function().strict {
                    self.bytecode_generator.append_delete_member_strict(iseq);
                } else {
                    self.bytecode_generator.append_delete_member(iseq);
                }
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
            }
            NodeBase::Index(ref parent, ref index) => {
                self.visit(&*parent, iseq, true)?;
                self.visit(&*index, iseq, true)?;
                self.save_source_pos(iseq);
                if self.current_function().strict {
                    self.bytecode_generator.append_delete_member_strict(iseq);
                } else {
                    self.bytecode_generator.append_delete_member(iseq);
                }
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
            }
            NodeBase::PrivateMember(_, _)
            | NodeBase::PrivateMemberInit(_, _, _)
            | NodeBase::PrivateAccessorInit(_, _, _) => {
                return Err(Error::new_general_error(
                    "Syntax error: private fields cannot be deleted.".to_string(),
                    expr.loc,
                ));
            }
            NodeBase::Identifier(_) => {
                if self.current_function().strict {
                    return Err(Error::new_general_error(
                        "Syntax error: Delete of an unqualified identifier in strict mode."
                            .to_string(),
                        expr.loc,
                    ));
                }
                if use_value {
                    self.bytecode_generator.append_push_bool(false, iseq);
                }
            }
            _ => {
                self.visit(expr, iseq, true)?;
                self.bytecode_generator.append_pop(iseq);
                if use_value {
                    self.bytecode_generator.append_push_bool(true, iseq);
                }
            }
        }

        Ok(())
    }

    fn visit_binary_op(
        &mut self,
        lhs: &Node,
        rhs: &Node,
        op: &BinOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        match op {
            &BinOp::LAnd => {
                self.visit(lhs, iseq, true)?;

                self.bytecode_generator.append_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_generator.append_jmp_if_false(0, iseq);

                self.bytecode_generator.append_pop(iseq);

                self.visit(rhs, iseq, true)?;

                let pos = iseq.len() as isize;
                self.bytecode_generator.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }

                return Ok(());
            }
            &BinOp::LOr => {
                self.visit(lhs, iseq, true)?;

                self.bytecode_generator.append_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_generator.append_jmp_if_true(0, iseq);

                self.bytecode_generator.append_pop(iseq);

                self.visit(rhs, iseq, true)?;

                let pos = iseq.len() as isize;
                self.bytecode_generator.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }

                return Ok(());
            }
            &BinOp::Coalesce => {
                self.visit(lhs, iseq, true)?;

                self.bytecode_generator.append_double(iseq);

                let lhs_cond_pos = iseq.len() as isize;
                self.bytecode_generator.append_jmp_if_not_nullish(0, iseq);

                self.bytecode_generator.append_pop(iseq);

                self.visit(rhs, iseq, true)?;

                let pos = iseq.len() as isize;
                self.bytecode_generator.replace_int32(
                    (pos - lhs_cond_pos) as i32 - 5,
                    &mut iseq[lhs_cond_pos as usize + 1..lhs_cond_pos as usize + 5],
                );

                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }

                return Ok(());
            }
            // http://www.ecma-international.org/ecma-262/9.0/index.html#sec-comma-operator
            &BinOp::Comma => {
                self.visit(lhs, iseq, false)?;
                self.visit(rhs, iseq, true)?;
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
                return Ok(());
            }
            _ => {}
        }

        self.visit(lhs, iseq, true)?;
        self.visit(rhs, iseq, true)?;

        match op {
            &BinOp::Add => self.bytecode_generator.append_add(iseq),
            &BinOp::Sub => self.bytecode_generator.append_sub(iseq),
            &BinOp::Mul => self.bytecode_generator.append_mul(iseq),
            &BinOp::Div => self.bytecode_generator.append_div(iseq),
            &BinOp::Rem => self.bytecode_generator.append_rem(iseq),
            &BinOp::Exp => self.bytecode_generator.append_exp(iseq),
            &BinOp::Eq => self.bytecode_generator.append_eq(iseq),
            &BinOp::Ne => self.bytecode_generator.append_ne(iseq),
            &BinOp::SEq => self.bytecode_generator.append_seq(iseq),
            &BinOp::SNe => self.bytecode_generator.append_sne(iseq),
            &BinOp::And => self.bytecode_generator.append_and(iseq),
            &BinOp::Or => self.bytecode_generator.append_or(iseq),
            &BinOp::Xor => self.bytecode_generator.append_xor(iseq),
            &BinOp::Lt => self.bytecode_generator.append_lt(iseq),
            &BinOp::Gt => self.bytecode_generator.append_gt(iseq),
            &BinOp::Le => self.bytecode_generator.append_le(iseq),
            &BinOp::Ge => self.bytecode_generator.append_ge(iseq),
            &BinOp::Instanceof => self.bytecode_generator.append_instanceof(iseq),
            &BinOp::In => self.bytecode_generator.append_in(iseq),
            &BinOp::Shl => self.bytecode_generator.append_shl(iseq),
            &BinOp::Shr => self.bytecode_generator.append_shr(iseq),
            &BinOp::ZFShr => self.bytecode_generator.append_zfshr(iseq),
            _ => unimplemented!(),
        }

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_ternary_op(
        &mut self,
        cond: &Node,
        then_exp: &Node,
        else_exp: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        self.visit(cond, iseq, true)?;

        let cond_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp_if_false(0, iseq);

        self.visit(then_exp, iseq, use_value)?;

        let then_end_pos = iseq.len() as isize;
        self.bytecode_generator.append_jmp(0, iseq);

        let pos = iseq.len() as isize;
        self.bytecode_generator.replace_int32(
            (pos - cond_pos) as i32 - 5,
            &mut iseq[cond_pos as usize + 1..cond_pos as usize + 5],
        );

        self.visit(else_exp, iseq, use_value)?;

        let pos = iseq.len() as isize;
        self.bytecode_generator.replace_int32(
            (pos - then_end_pos) as i32 - 5,
            &mut iseq[then_end_pos as usize + 1..then_end_pos as usize + 5],
        );

        Ok(())
    }

    fn visit_assign(
        &mut self,
        dst: &Node,
        src: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if self.current_function().strict {
            if let NodeBase::Identifier(ref name) = dst.base {
                if name == "eval" || name == "arguments" {
                    return Err(Error::new_general_error(
                        format!("Cannot assign to '{}' in strict mode", name),
                        dst.loc,
                    ));
                }
            }
        }

        self.visit(src, iseq, true)?;
        if let Some(name) = Self::inferred_name_for_target(dst) {
            self.append_inferred_name_if_needed(src, &name, iseq);
        }

        if use_value {
            self.bytecode_generator.append_double(iseq);
        }

        self.assign_stack_top_to(dst, iseq)?;

        Ok(())
    }

    fn inferred_name_for_target(target: &Node) -> Option<String> {
        match target.base {
            NodeBase::Identifier(ref name) => Some(name.clone()),
            _ => None,
        }
    }

    fn is_anonymous_name_inference_candidate(expr: &Node) -> bool {
        matches!(
            expr.base,
            NodeBase::FunctionExpr(None, _, _)
                | NodeBase::GeneratorFunctionExpr(None, _, _)
                | NodeBase::AsyncFunctionExpr(None, _, _)
                | NodeBase::AsyncGeneratorFunctionExpr(None, _, _)
                | NodeBase::ArrowFunction(_, _)
                | NodeBase::AsyncArrowFunction(_, _)
                | NodeBase::AnonymousClassExpr(_)
        )
    }

    fn append_inferred_name_if_needed(&mut self, expr: &Node, name: &String, iseq: &mut ByteCode) {
        if Self::is_anonymous_name_inference_candidate(expr) {
            self.bytecode_generator.append_set_function_name(name, iseq);
        }
    }

    fn visit_assign_op(
        &mut self,
        dst: &Node,
        src: &Node,
        op: &BinOp,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if let NodeBase::Identifier(ref name) = dst.base {
            if Self::is_simple_compound_op(op) {
                self.bytecode_generator
                    .append_get_value_keep_ref(name, iseq);
                self.visit(src, iseq, true)?;
                self.append_simple_compound_op(op, iseq)?;
                if use_value {
                    self.bytecode_generator.append_double(iseq);
                }
                self.bytecode_generator
                    .append_set_value_keep_ref(name, iseq);
                return Ok(());
            }
        }

        self.visit_binary_op(dst, src, op, iseq, true)?;
        if use_value {
            self.bytecode_generator.append_double(iseq);
        }
        self.assign_stack_top_to(dst, iseq)?;
        Ok(())
    }

    fn is_simple_compound_op(op: &BinOp) -> bool {
        matches!(
            op,
            BinOp::Add
                | BinOp::Sub
                | BinOp::Mul
                | BinOp::Div
                | BinOp::Rem
                | BinOp::And
                | BinOp::Or
                | BinOp::Xor
                | BinOp::Shl
                | BinOp::Shr
                | BinOp::ZFShr
        )
    }

    fn append_simple_compound_op(&mut self, op: &BinOp, iseq: &mut ByteCode) -> CodeGenResult {
        match op {
            BinOp::Add => self.bytecode_generator.append_add(iseq),
            BinOp::Sub => self.bytecode_generator.append_sub(iseq),
            BinOp::Mul => self.bytecode_generator.append_mul(iseq),
            BinOp::Div => self.bytecode_generator.append_div(iseq),
            BinOp::Rem => self.bytecode_generator.append_rem(iseq),
            BinOp::And => self.bytecode_generator.append_and(iseq),
            BinOp::Or => self.bytecode_generator.append_or(iseq),
            BinOp::Xor => self.bytecode_generator.append_xor(iseq),
            BinOp::Shl => self.bytecode_generator.append_shl(iseq),
            BinOp::Shr => self.bytecode_generator.append_shr(iseq),
            BinOp::ZFShr => self.bytecode_generator.append_zfshr(iseq),
            _ => {
                return Err(Error::new_unimplemented_error(
                    "compound assignment operator".to_string(),
                    self.loc,
                ))
            }
        }
        Ok(())
    }

    fn visit_call(
        &mut self,
        callee: &Node,
        args: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        let has_spread = args
            .iter()
            .any(|arg| matches!(arg.base, NodeBase::Spread(_)));

        if has_spread {
            self.bytecode_generator.append_push_seperator(iseq);
        }
        for arg in args.iter().rev() {
            self.visit(arg, iseq, true)?;
        }

        match callee.base {
            NodeBase::Member(ref parent, ref property_name) => {
                self.bytecode_generator
                    .append_push_const(self.factory.string(property_name.clone()), iseq);
                if matches!(parent.base, NodeBase::Identifier(ref name) if name == "super") {
                    self.save_source_pos(iseq);
                    if has_spread {
                        self.bytecode_generator
                            .append_call_super_method_spread(iseq);
                    } else {
                        self.bytecode_generator
                            .append_call_super_method(args.len() as u32, iseq);
                    }
                    if !use_value {
                        self.bytecode_generator.append_pop(iseq);
                    }
                    return Ok(());
                }
                self.visit(&*parent, iseq, true)?;
                self.save_source_pos(iseq);
                if has_spread {
                    self.bytecode_generator.append_call_method_spread(iseq);
                } else {
                    self.bytecode_generator
                        .append_call_method(args.len() as u32, iseq);
                }
            }
            NodeBase::PrivateMember(ref parent, ref property_name) => {
                self.bytecode_generator
                    .append_push_const(self.factory.string(property_name.clone()), iseq);
                self.visit(&*parent, iseq, true)?;
                self.save_source_pos(iseq);
                if has_spread {
                    self.bytecode_generator
                        .append_call_private_method_spread(iseq);
                } else {
                    self.bytecode_generator
                        .append_call_private_method(args.len() as u32, iseq);
                }
            }
            NodeBase::Index(ref parent, ref index) => {
                self.visit(&*index, iseq, true)?;
                self.visit(&*parent, iseq, true)?;
                self.save_source_pos(iseq);
                if has_spread {
                    self.bytecode_generator.append_call_method_spread(iseq);
                } else {
                    self.bytecode_generator
                        .append_call_method(args.len() as u32, iseq);
                }
            }
            _ => {
                self.visit(callee, iseq, true)?;
                self.save_source_pos(iseq);
                if has_spread {
                    self.bytecode_generator.append_call_spread(iseq);
                } else if matches!(callee.base, NodeBase::Identifier(ref name) if name == "eval") {
                    self.bytecode_generator
                        .append_call_direct_eval(args.len() as u32, iseq);
                } else {
                    self.bytecode_generator.append_call(args.len() as u32, iseq);
                }
            }
        }

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_super_call(
        &mut self,
        args: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        for arg in args.iter().rev() {
            self.visit(arg, iseq, true)?
        }
        let super_construct = self.factory.builtin_function("", builtins::super_construct);
        self.bytecode_generator
            .append_push_const(super_construct, iseq);
        self.save_source_pos(iseq);
        self.bytecode_generator.append_call(args.len() as u32, iseq);
        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }
        Ok(())
    }

    fn visit_super_call_from_arguments(
        &mut self,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        let super_construct = self
            .factory
            .builtin_function("", builtins::super_construct_arguments);
        self.bytecode_generator
            .append_push_const(super_construct, iseq);
        self.save_source_pos(iseq);
        self.bytecode_generator.append_call(0, iseq);
        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }
        Ok(())
    }

    fn visit_throw(&mut self, val: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        self.visit(val, iseq, true)?;

        if self.current_function().in_try_or_catch() {
            self.unwind_try_or_catch(iseq);
        } else if self.current_function().in_finally() {
            self.unwind_finally(iseq);
        }

        self.save_source_pos(iseq);
        self.bytecode_generator.append_throw(iseq);
        Ok(())
    }

    fn visit_return(&mut self, val: &Option<Box<Node>>, iseq: &mut ByteCode) -> CodeGenResult {
        if let Some(val) = val {
            self.visit(val, iseq, true)?
        } else {
            self.bytecode_generator.append_push_undefined(iseq);
        }

        if self.current_function().in_try_or_catch() {
            self.current_function()
                .get_last_try_or_catch()
                .as_try_or_catch_mut()
                .push(iseq.len() as usize);
            self.bytecode_generator.append_return_try(iseq);
        } else {
            self.bytecode_generator.append_return(iseq);
        }

        Ok(())
    }

    fn visit_yield(
        &mut self,
        val: &Option<Box<Node>>,
        is_yield_star: bool,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        if is_yield_star {
            if let Some(val) = val {
                self.visit(val, iseq, true)?;
            } else {
                self.bytecode_generator.append_push_undefined(iseq);
            }
            self.bytecode_generator.append_for_of_enumerate(iseq);

            let start = iseq.len() as isize;
            let next_pos = iseq.len();
            self.bytecode_generator.append_for_of_next_value(0, iseq);
            self.bytecode_generator.append_yield(iseq);
            self.bytecode_generator.append_pop(iseq);

            let loop_pos = iseq.len() as isize;
            self.bytecode_generator
                .append_jmp((start - loop_pos) as i32 - 5, iseq);

            let cleanup = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (cleanup - next_pos as isize) as i32 - 5,
                &mut iseq[next_pos + 1..next_pos + 5],
            );
            self.bytecode_generator.append_pop(iseq);
            if use_value {
                self.bytecode_generator.append_push_undefined(iseq);
            }
            return Ok(());
        }

        if let Some(val) = val {
            self.visit(val, iseq, true)?;
        } else {
            self.bytecode_generator.append_push_undefined(iseq);
        }
        self.bytecode_generator.append_yield(iseq);
        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }
        Ok(())
    }

    fn visit_new(&mut self, expr: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        let (callee, args) = match expr.base {
            NodeBase::Call(ref callee, ref args) => (&*callee, args),
            _ => unimplemented!(),
        };

        for arg in args.iter().rev() {
            self.visit(arg, iseq, true)?
        }

        match callee.base {
            NodeBase::Member(ref parent, ref property_name) => {
                self.visit(parent, iseq, true)?;
                let property = self.factory.string(property_name.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                self.bytecode_generator.append_get_member(iseq);
            }
            _ => {
                self.visit(callee, iseq, true)?;
            }
        }

        self.bytecode_generator.append_construct(args.len(), iseq);

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
        }

        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        properties: &Vec<PropertyDefinition>,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.bytecode_generator.append_push_seperator(iseq);
        let mut special_properties = SpecialProperties::default();
        let len = properties.len();
        for (i, property) in properties.iter().rev().enumerate() {
            use MethodDefinitionKind::*;
            use PropertyDefinition::*;
            use SpecialPropertyKind::*;
            match property {
                // { name }
                IdentifierReference(name) => {
                    self.save_source_pos(iseq);
                    self.bytecode_generator.append_get_value(name, iseq);
                    self.bytecode_generator
                        .append_push_const(self.factory.string(name.clone()), iseq);
                }
                // { name: val }
                Property(name, val) => {
                    self.visit(&val, iseq, true)?;
                    self.bytecode_generator
                        .append_push_const(self.factory.string(name.clone()), iseq);
                }
                // { [key]: val }
                ComputedProperty(key, val) => {
                    self.visit(&val, iseq, true)?;
                    self.visit(&key, iseq, true)?;
                }
                CoverInitializedName(_, _) => {
                    return Err(Error::new_general_error(
                        "Syntax error: invalid object initializer.".to_string(),
                        self.loc,
                    ));
                }
                // { get name(){ node }}
                // { set name(){ node }}
                MethodDefinition(kind, name, node) => {
                    match kind {
                        Ordinary => {}
                        Set => {
                            special_properties.insert(len - i - 1, Setter);
                        }
                        Get => {
                            special_properties.insert(len - i - 1, Getter);
                        }
                    };
                    self.visit(&node, iseq, true)?;
                    self.bytecode_generator
                        .append_push_const(self.factory.string(name.clone()), iseq);
                }
                // { get [key](){ node }}
                // { set [key](){ node }}
                // { [key](){ node }}
                ComputedMethodDefinition(kind, key, node) => {
                    match kind {
                        Ordinary => {}
                        Set => {
                            special_properties.insert(len - i - 1, Setter);
                        }
                        Get => {
                            special_properties.insert(len - i - 1, Getter);
                        }
                    };
                    self.visit(&node, iseq, true)?;
                    self.visit(&key, iseq, true)?;
                }
                // { ...node }
                SpreadObject(node) => {
                    special_properties.insert(len - i - 1, Spread);
                    self.visit(&node, iseq, true)?;
                    self.bytecode_generator.append_push_null(iseq);
                }
            }
        }

        let id = self
            .bytecode_generator
            .constant_table
            .add_object_literal_info(special_properties);

        self.bytecode_generator.append_create_object(id, iseq);

        Ok(())
    }

    fn visit_array_literal(&mut self, elems: &Vec<Node>, iseq: &mut ByteCode) -> CodeGenResult {
        self.bytecode_generator.append_push_seperator(iseq);
        for elem in elems.iter().rev() {
            if elem.base == NodeBase::Nope {
                self.bytecode_generator
                    .append_push_const(value::Value::empty(), iseq);
            } else {
                self.visit(elem, iseq, true)?;
            }
        }

        self.bytecode_generator.append_create_array(iseq);

        Ok(())
    }
}

impl<'a> CodeGenerator<'a> {
    fn apply_default_initializer(
        &mut self,
        init: &Option<Node>,
        inferred_name: Option<&String>,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        if let Some(init) = init {
            self.bytecode_generator.append_double(iseq);
            self.bytecode_generator.append_push_undefined(iseq);
            self.bytecode_generator.append_seq(iseq);
            let skip_default_pos = iseq.len();
            self.bytecode_generator.append_jmp_if_false(0, iseq);
            self.bytecode_generator.append_pop(iseq);
            self.visit(init, iseq, true)?;
            if let Some(name) = inferred_name {
                self.append_inferred_name_if_needed(init, name, iseq);
            }
            let end = iseq.len() as isize;
            self.bytecode_generator.replace_int32(
                (end - skip_default_pos as isize) as i32 - 5,
                &mut iseq[skip_default_pos + 1..skip_default_pos + 5],
            );
        }
        Ok(())
    }

    fn destructure_stack_top_to(
        &mut self,
        pattern: &Node,
        iseq: &mut ByteCode,
        is_binding: bool,
    ) -> CodeGenResult {
        match pattern.base {
            NodeBase::ArrayPattern(ref elements) => {
                self.bytecode_generator.append_for_of_enumerate(iseq);
                for element in elements {
                    match element {
                        ArrayPatternElement::Elision => {
                            self.bytecode_generator.append_iterator_next(iseq);
                            self.bytecode_generator.append_pop(iseq);
                        }
                        ArrayPatternElement::Element(target, init) => {
                            self.bytecode_generator.append_iterator_next(iseq);
                            let inferred_name = Self::inferred_name_for_target(target);
                            self.apply_default_initializer(init, inferred_name.as_ref(), iseq)?;
                            self.destructure_stack_top_to(target, iseq, is_binding)?;
                        }
                        ArrayPatternElement::Rest(target) => {
                            if is_binding
                                || matches!(
                                    target.base,
                                    NodeBase::ArrayPattern(_) | NodeBase::ObjectPattern(_)
                                )
                            {
                                self.bytecode_generator.append_iterator_rest_array(iseq);
                                self.destructure_stack_top_to(target, iseq, is_binding)?;
                            } else {
                                self.bytecode_generator
                                    .append_push_pending_iterator_close(iseq);
                                self.prepare_assignment_reference(target, iseq)?;
                                self.bytecode_generator
                                    .append_pop_pending_iterator_close(iseq);
                                self.bytecode_generator.append_iterator_rest_array(iseq);
                                self.bytecode_generator.append_set_pending_reference(iseq);
                            }
                            break;
                        }
                    }
                }
                self.bytecode_generator.append_iterator_close(iseq);
                self.bytecode_generator.append_pop(iseq);
            }
            NodeBase::ObjectPattern(ref properties) => {
                self.bytecode_generator
                    .append_require_object_coercible(iseq);
                let has_rest = properties
                    .iter()
                    .any(|property| matches!(property, ObjectPatternProperty::Rest(_)));
                let mut excluded_count = 0;
                for (index, property) in properties.iter().enumerate() {
                    match property {
                        ObjectPatternProperty::Property(key, target, init) => {
                            if has_rest {
                                self.bytecode_generator
                                    .append_push_const(self.factory.string(key.clone()), iseq);
                                self.bytecode_generator.append_object_rest_exclusion(iseq);
                                self.bytecode_generator.append_pop(iseq);
                                excluded_count += 1;
                            }
                            self.bytecode_generator.append_double(iseq);
                            self.bytecode_generator
                                .append_push_const(self.factory.string(key.clone()), iseq);
                            self.bytecode_generator.append_get_member(iseq);
                            let inferred_name = Self::inferred_name_for_target(target);
                            self.apply_default_initializer(init, inferred_name.as_ref(), iseq)?;
                            self.destructure_stack_top_to(target, iseq, is_binding)?;
                        }
                        ObjectPatternProperty::ComputedProperty(key, target, init) => {
                            if has_rest {
                                excluded_count += 1;
                            }
                            self.bytecode_generator.append_double(iseq);
                            self.visit(key, iseq, true)?;
                            if has_rest {
                                self.bytecode_generator.append_object_rest_exclusion(iseq);
                            }
                            self.bytecode_generator.append_get_member(iseq);
                            let inferred_name = Self::inferred_name_for_target(target);
                            self.apply_default_initializer(init, inferred_name.as_ref(), iseq)?;
                            self.destructure_stack_top_to(target, iseq, is_binding)?;
                        }
                        ObjectPatternProperty::Rest(target) => {
                            if index + 1 != properties.len() {
                                return Err(Error::new_general_error(
                                    "Syntax error: rest property must be last.".to_string(),
                                    target.loc,
                                ));
                            }
                            self.bytecode_generator.append_double(iseq);
                            self.bytecode_generator
                                .append_object_rest(excluded_count, iseq);
                            self.destructure_stack_top_to(target, iseq, is_binding)?;
                        }
                    }
                }
                self.bytecode_generator.append_pop(iseq);
            }
            _ => self.assign_stack_top_to(pattern, iseq)?,
        }

        Ok(())
    }

    fn assign_stack_top_to(&mut self, dst: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        match dst.base {
            NodeBase::ArrayPattern(_) | NodeBase::ObjectPattern(_) => {
                return self.destructure_stack_top_to(dst, iseq, false);
            }
            NodeBase::Identifier(ref name) => {
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_value(name, iseq);
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit(&*parent, iseq, true)?;
                let property = self.factory.string(property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_member(iseq);
            }
            NodeBase::PrivateMember(ref parent, ref property) => {
                self.visit(&*parent, iseq, true)?;
                let property = self.factory.string(property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_private_member(iseq);
            }
            NodeBase::PrivateMemberInit(ref parent, ref property, writable) => {
                self.visit(&*parent, iseq, true)?;
                let property = self.factory.string(property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                if writable {
                    self.bytecode_generator.append_define_private_member(iseq);
                } else {
                    self.bytecode_generator.append_define_private_method(iseq);
                }
            }
            NodeBase::PrivateAccessorInit(ref parent, ref property, is_getter) => {
                self.visit(&*parent, iseq, true)?;
                let property = self.factory.string(property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                if is_getter {
                    self.bytecode_generator.append_define_private_getter(iseq);
                } else {
                    self.bytecode_generator.append_define_private_setter(iseq);
                }
            }
            NodeBase::Index(ref parent, ref index) => {
                self.visit(&*parent, iseq, true)?;
                self.visit(&*index, iseq, true)?;
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_member(iseq);
            }
            _ => {
                return Err(Error::new_general_error(
                    "Reference error: Invalid left-hand side in assignment.".to_string(),
                    dst.loc,
                ));
            }
        }

        Ok(())
    }

    fn prepare_assignment_reference(&mut self, dst: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.save_source_pos(iseq);
                self.bytecode_generator
                    .append_make_binding_reference(name, iseq);
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit(&*parent, iseq, true)?;
                let property = self.factory.string(property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.bytecode_generator.append_make_object_reference(iseq);
            }
            NodeBase::Index(ref parent, ref index) => {
                self.visit(&*parent, iseq, true)?;
                self.visit(&*index, iseq, true)?;
                self.bytecode_generator.append_make_object_reference(iseq);
            }
            _ => {
                return Err(Error::new_general_error(
                    "Reference error: Invalid left-hand side in assignment.".to_string(),
                    dst.loc,
                ));
            }
        }
        Ok(())
    }

    fn current_function(&mut self) -> &mut FunctionInfo {
        self.function_stack.last_mut().unwrap()
    }

    fn consume_pending_labels(&mut self) -> Vec<String> {
        let labels = self.pending_labels.clone();
        self.pending_labels.clear();
        labels
    }

    /// Save the position in bytecode corresponds to the current node.
    fn save_source_pos(&mut self, iseq: &mut ByteCode) {
        let loc = self.loc;
        self.current_function()
            .to_source_pos
            .append(iseq.len(), loc);
    }

    fn unwind_try_or_catch(&mut self, iseq: &mut ByteCode) {
        for level in self.current_function().level.clone().iter().rev() {
            match level {
                &Level::TryOrCatch { .. } => break,
                &Level::Block { .. } => self.bytecode_generator.append_pop_env(iseq),
                _ => {}
            }
        }
    }

    fn unwind_finally(&mut self, iseq: &mut ByteCode) {
        for level in self.current_function().level.clone().iter().rev() {
            match level {
                &Level::Finally => break,
                &Level::Block { .. } => self.bytecode_generator.append_pop_env(iseq),
                _ => {}
            }
        }
    }

    fn unwind_loop(&mut self, name: &Option<String>, iseq: &mut ByteCode) {
        let mut count = 0;
        for level in self.current_function().level.iter().rev() {
            if FunctionInfo::is_loop_target(level, name) {
                break;
            }
            match level {
                &Level::Block { .. } => count += 1,
                _ => {}
            }
        }
        for _ in 0..count {
            self.bytecode_generator.append_pop_env(iseq);
        }
    }

    fn unwind_breakable(&mut self, name: &Option<String>, iseq: &mut ByteCode) {
        let mut count = 0;
        for level in self.current_function().level.iter().rev() {
            if FunctionInfo::is_breakable_target(level, name) {
                break;
            }
            match level {
                &Level::Block { .. } => count += 1,
                _ => {}
            }
        }
        for _ in 0..count {
            self.bytecode_generator.append_pop_env(iseq);
        }
    }
}

// Methods for Error handling

impl Error {
    pub fn new_general_error(msg: String, loc: SourceLoc) -> Self {
        Error {
            msg,
            loc,
            kind: ErrorKind::General,
        }
    }

    pub fn new_unimplemented_error(msg: String, loc: SourceLoc) -> Self {
        Error {
            msg,
            loc,
            kind: ErrorKind::Unimplemented,
        }
    }
}

// FunctionInfo

impl FunctionInfo {
    pub fn new(name: Option<String>, module_func_id: FunctionId) -> Self {
        FunctionInfo {
            name,
            strict: false,
            var_names: vec![],
            lex_names: vec![],
            const_names: vec![],
            func_decls: vec![],
            param_names: vec![],
            level: vec![Level::Function],
            exception_table: vec![],
            to_source_pos: ToSourcePos::new(module_func_id),
            module_func_id,
        }
    }

    pub fn in_try_or_catch(&self) -> bool {
        self.level
            .iter()
            .rev()
            .find(|level| match level {
                &Level::TryOrCatch { .. } => true,
                _ => false,
            })
            .is_some()
    }

    pub fn in_finally(&self) -> bool {
        self.level
            .iter()
            .rev()
            .find(|level| *level == &Level::Finally)
            .is_some()
    }

    pub fn has_loop(&self) -> bool {
        self.level
            .iter()
            .rev()
            .any(|level| matches!(level, &Level::Loop { .. }))
    }

    pub fn has_breakable(&self) -> bool {
        self.level
            .iter()
            .rev()
            .any(|level| matches!(level, &Level::Loop { .. } | &Level::Switch { .. }))
    }

    pub fn has_loop_target(&self, name: &Option<String>) -> bool {
        self.level
            .iter()
            .rev()
            .any(|level| Self::is_loop_target(level, name))
    }

    pub fn has_breakable_target(&self, name: &Option<String>) -> bool {
        self.level
            .iter()
            .rev()
            .any(|level| Self::is_breakable_target(level, name))
    }

    pub fn get_last_try_or_catch(&mut self) -> &mut Level {
        self.level
            .iter_mut()
            .rev()
            .find(|level| match level {
                &Level::TryOrCatch { .. } => true,
                _ => false,
            })
            .unwrap()
    }

    pub fn find_last_try_or_catch(&mut self) -> Option<&mut Level> {
        self.level.iter_mut().rev().find(|level| match level {
            &Level::TryOrCatch { .. } => true,
            _ => false,
        })
    }

    pub fn get_last_loop(&mut self) -> &mut Level {
        self.level
            .iter_mut()
            .rev()
            .find(|level| match level {
                &Level::Loop { .. } => true,
                _ => false,
            })
            .unwrap()
    }

    pub fn get_last_loop_target(&mut self, name: &Option<String>) -> &mut Level {
        self.level
            .iter_mut()
            .rev()
            .find(|level| Self::is_loop_target(level, name))
            .unwrap()
    }

    pub fn get_last_breakable(&mut self) -> &mut Level {
        self.level
            .iter_mut()
            .rev()
            .find(|level| match level {
                &Level::Loop { .. } | &Level::Switch { .. } => true,
                _ => false,
            })
            .unwrap()
    }

    pub fn get_last_breakable_target(&mut self, name: &Option<String>) -> &mut Level {
        self.level
            .iter_mut()
            .rev()
            .find(|level| Self::is_breakable_target(level, name))
            .unwrap()
    }

    pub fn get_last_block(&mut self) -> Option<&mut Level> {
        self.level.iter_mut().rev().find(|level| match level {
            &Level::Block { .. } => true,
            _ => false,
        })
    }

    fn is_loop_target(level: &Level, name: &Option<String>) -> bool {
        match (level, name) {
            (Level::Loop { .. }, None) => true,
            (Level::Loop { labels, .. }, Some(name)) => labels.iter().any(|label| label == name),
            _ => false,
        }
    }

    fn is_breakable_target(level: &Level, name: &Option<String>) -> bool {
        match (level, name) {
            (Level::Loop { .. } | Level::Switch { .. }, None) => true,
            (Level::Loop { labels, .. } | Level::Switch { labels, .. }, Some(name)) => {
                labels.iter().any(|label| label == name)
            }
            _ => false,
        }
    }
}

fn is_strict_body(node: &Node) -> bool {
    let list = match node.base {
        NodeBase::StatementList(ref list) | NodeBase::Block(ref list) => list,
        _ => return false,
    };
    for node in list {
        match node.base {
            NodeBase::String(ref string) if string == "use strict" => return true,
            NodeBase::String(_) => {}
            _ => return false,
        }
    }
    false
}

fn is_strict_reserved_word(name: &str) -> bool {
    matches!(
        name,
        "implements"
            | "interface"
            | "let"
            | "package"
            | "private"
            | "protected"
            | "public"
            | "static"
            | "yield"
    )
}

// Level

impl Level {
    pub fn new_function_level() -> Self {
        Level::Function
    }

    pub fn new_block_level() -> Self {
        Level::Block { names: vec![] }
    }

    pub fn new_try_or_catch_level() -> Self {
        Level::TryOrCatch {
            finally_jmp_instr_pos: vec![],
        }
    }

    pub fn new_switch_level(labels: Vec<String>) -> Self {
        Level::Switch {
            labels,
            break_jmp_instr_pos: vec![],
        }
    }

    pub fn as_block(self) -> Vec<String> {
        match self {
            Level::Block { names } => names,
            _ => panic!(),
        }
    }

    pub fn as_block_mut(&mut self) -> &mut Vec<String> {
        match self {
            Level::Block { ref mut names } => names,
            _ => panic!(),
        }
    }

    pub fn as_loop(self) -> (Vec<usize>, Vec<usize>) {
        match self {
            Level::Loop {
                break_jmp_instr_pos,
                continue_jmp_instr_pos,
                ..
            } => (break_jmp_instr_pos, continue_jmp_instr_pos),
            _ => panic!(),
        }
    }

    pub fn as_loop_mut(&mut self) -> (&mut Vec<usize>, &mut Vec<usize>) {
        match self {
            Level::Loop {
                ref mut break_jmp_instr_pos,
                ref mut continue_jmp_instr_pos,
                ..
            } => (break_jmp_instr_pos, continue_jmp_instr_pos),
            _ => panic!(),
        }
    }

    pub fn as_breakable_mut(&mut self) -> &mut Vec<usize> {
        match self {
            Level::Loop {
                ref mut break_jmp_instr_pos,
                ..
            }
            | Level::Switch {
                ref mut break_jmp_instr_pos,
                ..
            } => break_jmp_instr_pos,
            _ => panic!(),
        }
    }

    pub fn replace_break_and_continue(
        self,
        bytecode_generator: &mut ByteCodeGenerator,
        iseq: &mut ByteCode,
        break_dst: isize,
        continue_dst: isize,
    ) {
        let (break_jmp_instr_pos, continue_jmp_instr_pos) = self.as_loop();
        for instr_pos in break_jmp_instr_pos {
            bytecode_generator.replace_int32(
                (break_dst - instr_pos as isize) as i32 - 5,
                &mut iseq[instr_pos as usize + 1..instr_pos as usize + 5],
            );
        }
        for instr_pos in continue_jmp_instr_pos {
            bytecode_generator.replace_int32(
                (continue_dst - instr_pos as isize) as i32 - 5,
                &mut iseq[instr_pos as usize + 1..instr_pos as usize + 5],
            );
        }
    }

    pub fn as_try_or_catch(self) -> Vec<usize> {
        match self {
            Level::TryOrCatch {
                finally_jmp_instr_pos,
            } => finally_jmp_instr_pos,
            _ => panic!(),
        }
    }

    pub fn as_try_or_catch_mut(&mut self) -> &mut Vec<usize> {
        match self {
            Level::TryOrCatch {
                ref mut finally_jmp_instr_pos,
            } => finally_jmp_instr_pos,
            _ => panic!(),
        }
    }

    pub fn set_jmp_to_finally(
        self,
        dst: usize,
        bytecode_generator: &mut ByteCodeGenerator,
        iseq: &mut ByteCode,
    ) -> bool {
        let finally_jmp_instr_pos = self.as_try_or_catch();
        let mut has_return_from_try_or_catch = false;
        for instr_pos in finally_jmp_instr_pos {
            has_return_from_try_or_catch |= iseq[instr_pos] == VMInst::RETURN_TRY;
            assert!(match iseq[instr_pos] {
                VMInst::RETURN_TRY | VMInst::JMP => true,
                _ => false,
            });
            bytecode_generator.replace_int32(
                (dst - instr_pos) as i32 - 5,
                &mut iseq[instr_pos as usize + 1..instr_pos as usize + 5],
            );
        }
        has_return_from_try_or_catch
    }
}

impl ToSourcePos {
    pub fn new(module_func_id: FunctionId) -> Self {
        Self {
            module_func_id,
            table: vec![],
        }
    }

    pub fn append(&mut self, bp: usize, node_loc: SourceLoc) {
        self.table.push((bp, node_loc));
    }

    pub fn func_id(&self) -> FunctionId {
        self.module_func_id
    }

    pub fn get_node_loc(&self, bytecode_offset: usize) -> Option<SourceLoc> {
        for (bp, np) in &self.table {
            if *bp == bytecode_offset {
                return Some(*np);
            }
        }
        None
    }
}
