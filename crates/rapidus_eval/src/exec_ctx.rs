use crate::lexical_env::LexicalEnv;

#[derive(Debug, Clone)]
pub struct ExecutionCtx {
    cur_lexical_env: LexicalEnv,
}

impl ExecutionCtx {
    pub const fn new() -> Self {
        Self {
            cur_lexical_env: LexicalEnv::new(),
        }
    }

    pub fn cur_lexical_env(&self) -> &LexicalEnv {
        &self.cur_lexical_env
    }

    pub fn cur_lexical_env_mut(&mut self) -> &mut LexicalEnv {
        &mut self.cur_lexical_env
    }
}
