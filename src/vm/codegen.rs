use crate::bytecode_gen::{ByteCode, ByteCodeGenerator, VMInst};
use crate::gc::MemoryAllocator;
use crate::id::get_unique_id;
use crate::node::{
    BinOp, FormalParameter, FormalParameters, MethodDefinitionKind, Node, NodeBase,
    PropertyDefinition, UnaryOp, VarKind,
};
use crate::vm::constant::{ConstantTable, SpecialProperties, SpecialPropertyKind};
use crate::vm::jsvalue::function::{DestinationKind, Exception, ThisMode, UserFunctionInfo};
use crate::vm::jsvalue::value::Value;
use crate::vm::jsvalue::{prototype, value};
use rustc_hash::FxHashMap;

pub type CodeGenResult = Result<(), Error>;

#[derive(Clone, Debug)]
pub struct Error {
    pub msg: String,
    pub token_pos: usize,
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
    pub memory_allocator: &'a mut MemoryAllocator,
    pub object_prototypes: &'a prototype::ObjectPrototypes,
    pub function_stack: Vec<FunctionInfo>,
    pub to_source_map: FxHashMap<usize, ToSourcePos>,
    /// A position in the bytecode of the current node.
    pub node_pos: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: Option<String>,
    pub param_names: Vec<String>,
    pub var_names: Vec<String>,
    pub lex_names: Vec<String>,
    pub func_decls: Vec<Value>,
    pub level: Vec<Level>,
    pub exception_table: Vec<Exception>,
    pub to_source_pos: ToSourcePos,
}

#[derive(Debug, Clone)]
pub struct ToSourcePos {
    table: Vec<(usize, usize)>,
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
        break_jmp_instr_pos: Vec<usize>,
        continue_jmp_instr_pos: Vec<usize>,
    },
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        constant_table: &'a mut ConstantTable,
        memory_allocator: &'a mut MemoryAllocator,
        object_prototypes: &'a prototype::ObjectPrototypes,
    ) -> Self {
        CodeGenerator {
            bytecode_generator: ByteCodeGenerator::new(constant_table),
            object_prototypes,
            memory_allocator,
            function_stack: vec![FunctionInfo::new(None) /* = global */],
            to_source_map: FxHashMap::default(),
            node_pos: 0,
        }
    }

    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<FunctionInfo, Error> {
        self.visit(node, iseq, use_value)?;
        self.bytecode_generator.append_end(iseq);

        let global_info = self.function_stack.pop().unwrap();
        self.to_source_map
            .insert(0, global_info.to_source_pos.clone());

        Ok(global_info)
    }
}

// Visit methods for each Node

impl<'a> CodeGenerator<'a> {
    fn visit(&mut self, node: &Node, iseq: &mut ByteCode, use_value: bool) -> CodeGenResult {
        self.node_pos = node.pos;
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
            NodeBase::For(ref init, ref cond, ref step, ref body) => {
                self.visit_for(&*init, &*cond, &*step, &*body, iseq)?
            }
            NodeBase::Break(ref name) => self.visit_break(name, iseq)?,
            NodeBase::Continue(ref name) => self.visit_continue(name, iseq)?,
            NodeBase::Try(ref try_clause, ref catch, ref param, ref finally) => {
                self.visit_try(&*try_clause, &*catch, &*param, &*finally, iseq)?
            }
            NodeBase::FunctionDecl(ref name, ref params, ref body) => {
                self.visit_function_decl(name, params, &*body)?
            }
            NodeBase::FunctionExpr(ref name, ref params, ref body) => {
                self.visit_function_expr(name, params, &*body, true, iseq, use_value)?
            }
            NodeBase::ArrowFunction(ref params, ref body) => {
                self.visit_function_expr(&None, params, &*body, false, iseq, use_value)?
            }
            NodeBase::VarDecl(ref name, ref init, ref kind) => {
                self.visit_var_decl(node, name, init, kind, iseq)?
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit_member(&*parent, property, iseq, use_value)?
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
            NodeBase::Call(ref callee, ref args) => {
                self.visit_call(&*callee, args, iseq, use_value)?
            }
            NodeBase::Throw(ref val) => self.visit_throw(val, iseq)?,
            NodeBase::Return(ref val) => self.visit_return(val, iseq)?,
            NodeBase::New(ref expr) => self.visit_new(&*expr, iseq, use_value)?,
            NodeBase::Object(ref properties) => self.visit_object_literal(properties, iseq)?,
            NodeBase::Array(ref elems) => self.visit_array_literal(elems, iseq)?,
            NodeBase::Identifier(ref name) => {
                self.save_source_pos(iseq);
                self.bytecode_generator.append_get_value(name, iseq);
                if !use_value {
                    self.bytecode_generator.append_pop(iseq);
                }
            }
            // NodeBase::Undefined => {
            //     if use_value {
            //         self.bytecode_generator.append_push_undefined(iseq);
            //     }
            // }
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
                        .append_push_const(Value::string(self.memory_allocator, s.clone()), iseq)
                }
            }
            NodeBase::Number(n) => {
                if use_value {
                    self.bytecode_generator.append_push_number(n, iseq)
                }
            }
            NodeBase::Boolean(b) => {
                if use_value {
                    self.bytecode_generator.append_push_bool(b, iseq)
                }
            }
            NodeBase::Nope => {
                if use_value {
                    self.bytecode_generator
                        .append_push_const(Value::empty(), iseq)
                }
            }
            NodeBase::TernaryOp(ref condition, ref then_clause, ref else_clause) => {
                self.visit_ternary_op(&*condition, &*then_clause, &*else_clause, iseq, use_value)?
            }
            ref e => unimplemented!("{:?}", e),
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
        self.current_function().level.push(Level::Loop {
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

    pub fn visit_for(
        &mut self,
        init: &Node,
        cond: &Node,
        step: &Node,
        body: &Node,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        self.current_function().level.push(Level::Loop {
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

    pub fn visit_break(&mut self, _name: &Option<String>, iseq: &mut ByteCode) -> CodeGenResult {
        self.unwind_loop(iseq);

        let break_instr_pos = iseq.len();
        self.bytecode_generator.append_jmp(0, iseq);

        self.current_function()
            .get_last_loop()
            .as_loop_mut()
            .0
            .push(break_instr_pos);

        Ok(())
    }

    pub fn visit_continue(&mut self, _name: &Option<String>, iseq: &mut ByteCode) -> CodeGenResult {
        let continue_instr_pos = iseq.len();
        self.bytecode_generator.append_jmp(0, iseq);

        self.current_function()
            .get_last_loop()
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
            let param_name = match param.base {
                NodeBase::Identifier(ref name) => name.clone(),
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
            self.current_function().level.push(Level::Block {
                names: vec![param_name.clone()],
            });
            self.save_source_pos(iseq);
            self.bytecode_generator.append_set_value(&param_name, iseq);

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
    ) -> CodeGenResult {
        let func = self.visit_function(Some(name.clone()), params, body, true)?;
        self.current_function().var_names.push(name.clone());
        self.current_function().func_decls.push(func);
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

        let func = self.visit_function(name.clone(), params, body, arrow_function)?;
        self.bytecode_generator.append_push_const(func, iseq);
        self.bytecode_generator.append_set_outer_env(iseq);

        Ok(())
    }

    fn visit_function(
        &mut self,
        name: Option<String>,
        params: &FormalParameters,
        body: &Node,
        arrow_function: bool,
    ) -> Result<Value, Error> {
        self.function_stack.push(FunctionInfo::new(name));

        let mut func_iseq = vec![];

        self.visit(body, &mut func_iseq, false)?;

        self.bytecode_generator
            .append_push_undefined(&mut func_iseq);
        self.bytecode_generator.append_return(&mut func_iseq);

        let params = params
            .clone()
            .iter()
            .map(
                |FormalParameter {
                     name,
                     is_rest_param,
                     ..
                 }| value::FunctionParameter {
                    name: name.clone(),
                    rest_param: *is_rest_param,
                },
            )
            .collect();

        let function_info = self.function_stack.pop().unwrap();

        let id = get_unique_id();

        self.to_source_map.insert(id, function_info.to_source_pos);

        Ok(Value::function(
            self.memory_allocator,
            self.object_prototypes,
            function_info.name,
            UserFunctionInfo {
                id,
                params,
                var_names: function_info.var_names,
                lex_names: function_info.lex_names,
                func_decls: function_info.func_decls,
                constructible: arrow_function,
                this_mode: if arrow_function {
                    ThisMode::Global
                } else {
                    ThisMode::Lexical
                },
                code: func_iseq,
                exception_table: function_info.exception_table,
                outer: None,
            },
        ))
    }

    pub fn visit_var_decl(
        &mut self,
        node: &Node,
        name: &String,
        init: &Option<Box<Node>>,
        kind: &VarKind,
        iseq: &mut ByteCode,
    ) -> CodeGenResult {
        fn let_decl(codegen: &mut CodeGenerator, node: &Node, name: String) -> CodeGenResult {
            fn check_duplicate(
                names: &mut Vec<String>,
                name: String,
                node: &Node,
            ) -> CodeGenResult {
                if names.iter().find(|declared| *declared == &name).is_some() {
                    return Err(Error::new_general_error(
                        format!("Identifier '{}' has already been declared", name),
                        node.pos,
                    ));
                }
                names.push(name);
                Ok(())
            }
            let cur_func = codegen.current_function();
            if let Some(ref mut block) = cur_func.get_last_block() {
                return check_duplicate(block.as_block_mut(), name, node);
            }
            check_duplicate(&mut cur_func.lex_names, name, node)
        }

        // let mut is_initialized = false;

        if let &Some(ref init) = init {
            self.visit(&*init, iseq, true)?;
            self.save_source_pos(iseq);
            self.bytecode_generator.append_set_value(name, iseq);
            // is_initialized = true;
        }

        match kind {
            VarKind::Var => {
                self.current_function().var_names.push(name.clone());
            }
            VarKind::Let => let_decl(self, node, name.clone())?,
            // TODO: Const needs double-assignment check
            VarKind::Const => let_decl(self, node, name.clone())?,
        }

        Ok(())
    }

    fn visit_member(
        &mut self,
        parent: &Node,
        member: &String,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        self.visit(parent, iseq, true)?;
        let property = Value::string(self.memory_allocator, member.clone());
        self.bytecode_generator.append_push_const(property, iseq);
        self.save_source_pos(iseq);
        self.bytecode_generator.append_get_member(iseq);

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

        if !use_value {
            self.bytecode_generator.append_pop(iseq);
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
        self.visit(cond, iseq, use_value)?;

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
        self.visit(src, iseq, true)?;

        if use_value {
            self.bytecode_generator.append_double(iseq);
        }

        self.assign_stack_top_to(dst, iseq)?;

        Ok(())
    }

    fn visit_call(
        &mut self,
        callee: &Node,
        args: &Vec<Node>,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> CodeGenResult {
        for arg in args.iter().rev() {
            self.visit(arg, iseq, true)?
        }

        match callee.base {
            NodeBase::Member(ref parent, ref property_name) => {
                self.bytecode_generator.append_push_const(
                    Value::string(self.memory_allocator, property_name.clone()),
                    iseq,
                );
                self.visit(&*parent, iseq, true)?;
                self.save_source_pos(iseq);
                self.bytecode_generator
                    .append_call_method(args.len() as u32, iseq);
            }
            _ => {
                self.visit(callee, iseq, true)?;
                self.save_source_pos(iseq);
                self.bytecode_generator.append_call(args.len() as u32, iseq);
            }
        }

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
                let property = Value::string(self.memory_allocator, property_name.clone());
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
        let mut special_properties = SpecialProperties::default();
        let len = properties.len();

        for (i, property) in properties.iter().enumerate() {
            match property {
                PropertyDefinition::IdentifierReference(name) => {
                    self.save_source_pos(iseq);
                    self.bytecode_generator.append_get_value(name, iseq);
                    self.bytecode_generator.append_push_const(
                        Value::string(self.memory_allocator, name.clone()),
                        iseq,
                    );
                }
                PropertyDefinition::Property(name, node) => {
                    self.visit(&node, iseq, true)?;
                    self.bytecode_generator.append_push_const(
                        Value::string(self.memory_allocator, name.clone()),
                        iseq,
                    );
                }
                PropertyDefinition::MethodDefinition(kind, name, node) => {
                    match kind {
                        MethodDefinitionKind::Ordinary => {}
                        MethodDefinitionKind::Set => {
                            special_properties.insert(len - i - 1, SpecialPropertyKind::Setter);
                        }
                        MethodDefinitionKind::Get => {
                            special_properties.insert(len - i - 1, SpecialPropertyKind::Getter);
                        }
                    };
                    self.visit(&node, iseq, true)?;
                    self.bytecode_generator.append_push_const(
                        Value::string(self.memory_allocator, name.clone()),
                        iseq,
                    );
                }
            }
        }

        let id = self
            .bytecode_generator
            .constant_table
            .add_object_literal_info(len, special_properties);

        self.bytecode_generator.append_create_object(id, iseq);

        Ok(())
    }

    fn visit_array_literal(&mut self, elems: &Vec<Node>, iseq: &mut ByteCode) -> CodeGenResult {
        for elem in elems.iter().rev() {
            self.visit(elem, iseq, true)?;
        }

        self.bytecode_generator
            .append_create_array(elems.len() as usize, iseq);

        Ok(())
    }
}

impl<'a> CodeGenerator<'a> {
    fn assign_stack_top_to(&mut self, dst: &Node, iseq: &mut ByteCode) -> CodeGenResult {
        match dst.base {
            NodeBase::Identifier(ref name) => {
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_value(name, iseq);
            }
            NodeBase::Member(ref parent, ref property) => {
                self.visit(&*parent, iseq, true)?;
                let property = Value::string(self.memory_allocator, property.clone());
                self.bytecode_generator.append_push_const(property, iseq);
                self.save_source_pos(iseq);
                self.bytecode_generator.append_set_member(iseq);
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
                    dst.pos,
                ));
            }
        }

        Ok(())
    }

    fn current_function(&mut self) -> &mut FunctionInfo {
        self.function_stack.last_mut().unwrap()
    }

    /// Save the position in bytecode corresponds to the current node.
    fn save_source_pos(&mut self, iseq: &mut ByteCode) {
        let node_pos = self.node_pos;
        self.current_function()
            .to_source_pos
            .append(iseq.len(), node_pos);
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

    fn unwind_loop(&mut self, iseq: &mut ByteCode) {
        let mut count = 0;
        for level in self.current_function().level.iter().rev() {
            match level {
                &Level::Loop { .. } => break,
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
    pub fn new_general_error(msg: String, token_pos: usize) -> Self {
        Error {
            msg,
            token_pos,
            kind: ErrorKind::General,
        }
    }

    pub fn new_unimplemented_error(msg: String, token_pos: usize) -> Self {
        Error {
            msg,
            token_pos,
            kind: ErrorKind::Unimplemented,
        }
    }
}

// FunctionInfo

impl FunctionInfo {
    pub fn new(name: Option<String>) -> Self {
        FunctionInfo {
            name,
            var_names: vec![],
            lex_names: vec![],
            func_decls: vec![],
            param_names: vec![],
            level: vec![Level::Function],
            exception_table: vec![],
            to_source_pos: ToSourcePos::new(),
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

    pub fn get_last_block(&mut self) -> Option<&mut Level> {
        self.level.iter_mut().rev().find(|level| match level {
            &Level::Block { .. } => true,
            _ => false,
        })
    }
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
            } => (break_jmp_instr_pos, continue_jmp_instr_pos),
            _ => panic!(),
        }
    }

    pub fn as_loop_mut(&mut self) -> (&mut Vec<usize>, &mut Vec<usize>) {
        match self {
            Level::Loop {
                ref mut break_jmp_instr_pos,
                ref mut continue_jmp_instr_pos,
            } => (break_jmp_instr_pos, continue_jmp_instr_pos),
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
    pub fn new() -> Self {
        Self { table: vec![] }
    }

    pub fn append(&mut self, bp: usize, np: usize) {
        self.table.push((bp, np));
    }

    pub fn get_node_pos(&self, bytecode_offset: usize) -> Option<usize> {
        for (bp, np) in &self.table {
            if *bp == bytecode_offset {
                return Some(*np);
            }
        }
        None
    }
}
