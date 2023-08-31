use ecow::EcoString;

use crate::{
    bytecode::{code::Code, insn},
    env::{Binding, Environment, ModuleEnv},
    error::Error,
    exec_ctx::ExecutionCtx,
    value::JsValue,
};

// TODO: What is the relationship between this and Realm?
pub struct EvalCtx {
    exec_ctx_stack: Vec<ExecutionCtx>,
    env_stack: Vec<Environment>,
    stack: Vec<JsValue>,
    bindings: Vec<JsValue>,
}

impl Default for EvalCtx {
    fn default() -> Self {
        Self {
            exec_ctx_stack: vec![ExecutionCtx::new(Code::new(), 0)],
            env_stack: vec![Environment::Module(ModuleEnv::new())],
            stack: Vec::new(),
            bindings: Vec::new(),
        }
    }
}

impl EvalCtx {
    pub fn new(exec_ctx: ExecutionCtx, env: Environment, bindings: Vec<JsValue>) -> Self {
        Self {
            exec_ctx_stack: vec![exec_ctx],
            env_stack: vec![env],
            stack: Vec::new(),
            bindings,
        }
    }

    pub fn run(&mut self) -> Result<JsValue, Error> {
        let ctx = self.exec_ctx_stack.last_mut().unwrap();
        let size = ctx.code().len();
        while ctx.pc() < size {
            let [opcode]: [u8; 1] = ctx.code().get(ctx.pc()).unwrap();
            let opcode = insn::Opcode(opcode);
            match opcode {
                insn::CONST_F64 => {
                    let val: [u8; 8] = ctx.code().get(ctx.pc() + 1).unwrap();
                    let val = f64::from_le_bytes(val);
                    self.stack.push(JsValue::f64(val));
                }
                insn::CONST_LIT => {
                    let idx: [u8; 4] = ctx.code().get(ctx.pc() + 1).unwrap();
                    let idx = u32::from_le_bytes(idx);
                    let val = ctx.code().get_lit(idx as usize).unwrap();
                    self.stack.push(val);
                }
                insn::ADD | insn::SUB | insn::MUL | insn::DIV | insn::MOD => {
                    let rhs = self.stack.pop().unwrap().as_f64().unwrap();
                    let lhs = self.stack.pop().unwrap().as_f64().unwrap();
                    match opcode {
                        insn::ADD => self.stack.push(JsValue::f64(lhs + rhs)),
                        insn::SUB => self.stack.push(JsValue::f64(lhs - rhs)),
                        insn::MUL => self.stack.push(JsValue::f64(lhs * rhs)),
                        insn::DIV => self.stack.push(JsValue::f64(lhs / rhs)),
                        insn::MOD => self.stack.push(JsValue::f64(lhs % rhs)),
                        _ => unreachable!(),
                    }
                }
                insn::NULL => self.stack.push(JsValue::null()),
                insn::NEW_MUT_BINDING => {
                    let idx: [u8; 4] = ctx.code().get(ctx.pc() + 1).unwrap();
                    let idx = u32::from_le_bytes(idx);
                    let name = ctx.code().get_ident(idx as usize).unwrap();
                    create_mutable_binding(
                        &mut self.env_stack,
                        &mut self.bindings,
                        name.to_owned(),
                    );
                }
                insn::SET_BINDING => {
                    let idx: [u8; 4] = ctx.code().get(ctx.pc() + 1).unwrap();
                    let idx = u32::from_le_bytes(idx);
                    let name = ctx.code().get_ident(idx as usize).unwrap();
                    let val = self.stack.pop().unwrap();
                    set_binding_value(ctx, &mut self.env_stack, &mut self.bindings, name, val);
                }
                insn::GET_BINDING => {
                    let idx: [u8; 4] = ctx.code().get(ctx.pc() + 1).unwrap();
                    let idx = u32::from_le_bytes(idx);
                    let name = ctx.code().get_ident(idx as usize).unwrap();
                    let val = get_binding_value(ctx, &self.env_stack, &self.bindings, name)
                        .map_or_else(
                            || Err(Error::ReferenceError),
                            |b| b.ok_or(Error::ReferenceError),
                        )?;
                    self.stack.push(val);
                }
                _ => return Err(Error::Todo),
            }
            *ctx.pc_mut() += opcode.total_bytes();
        }
        Ok(self.stack.pop().unwrap_or(JsValue::undefined()))
    }
}

fn create_mutable_binding(
    env_stack: &mut Vec<Environment>,
    bindings: &mut Vec<JsValue>,
    name: impl Into<EcoString>,
) {
    let env = &mut env_stack.last_mut().unwrap();
    let bind_idx = bindings.len();
    env.bindings_mut()
        .insert(name.into(), Binding::new(bind_idx, true));
    bindings.push(JsValue::undefined());
}

fn get_binding_value(
    exec_ctx: &ExecutionCtx,
    env_stack: &[Environment],
    bindings: &[JsValue],
    name: impl AsRef<str>,
) -> Option<Option<JsValue>> {
    lookup_binding(exec_ctx, env_stack, name).map(|binding| {
        if binding.initialized() {
            Some(bindings[binding.idx()])
        } else {
            None
        }
    })
}

fn set_binding_value(
    exec_ctx: &ExecutionCtx,
    env_stack: &mut Vec<Environment>,
    bindings: &mut Vec<JsValue>,
    name: impl AsRef<str>,
    value: JsValue,
) {
    let binding = lookup_binding_mut(exec_ctx, env_stack, name).unwrap();
    assert!(binding.mutable());
    assert!(!binding.initialized());
    binding.set_initialized(true);
    let idx = binding.idx();
    bindings[idx] = value;
}

fn lookup_binding(
    exec_ctx: &ExecutionCtx,
    env_stack: &[Environment],
    name: impl AsRef<str>,
) -> Option<Binding> {
    let mut envs = env_stack[exec_ctx.base_env_idx()..].iter().rev();
    envs.find_map(|env| env.bindings().get(name.as_ref()).copied())
}

fn lookup_binding_mut<'a>(
    exec_ctx: &ExecutionCtx,
    env_stack: &'a mut [Environment],
    name: impl AsRef<str>,
) -> Option<&'a mut Binding> {
    let mut envs = env_stack[exec_ctx.base_env_idx()..].iter_mut().rev();
    envs.find_map(|env| env.bindings_mut().get_mut(name.as_ref()))
}
