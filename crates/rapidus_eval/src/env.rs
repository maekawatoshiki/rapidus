use ecow::EcoString;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub enum Environment {
    Module(ModuleEnv),
    // TODO:
    // Lexical(LexicalEnv),
    // Global(GlobalEnv),
    // Object(ObjectEnv),
    // Function(FunctionEnv),
}

#[derive(Debug, Clone, Copy)]
pub struct Binding {
    idx: usize,
    mutable: bool,
    initialized: bool,
}

#[derive(Debug, Clone)]
pub struct ModuleEnv {
    bindings: FxHashMap<EcoString, Binding>,
}

impl Environment {
    pub fn bindings(&self) -> &FxHashMap<EcoString, Binding> {
        match self {
            Self::Module(env) => env.bindings(),
        }
    }

    pub fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, Binding> {
        match self {
            Self::Module(env) => env.bindings_mut(),
        }
    }
}

impl ModuleEnv {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }

    pub fn bindings(&self) -> &FxHashMap<EcoString, Binding> {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, Binding> {
        &mut self.bindings
    }
}

impl Binding {
    pub fn new(idx: usize, mutable: bool) -> Self {
        Self {
            idx,
            mutable,
            initialized: false,
        }
    }

    pub fn idx(&self) -> usize {
        self.idx
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }

    pub fn initialized(&self) -> bool {
        self.initialized
    }

    pub fn set_initialized(&mut self, initialized: bool) -> &mut Self {
        self.initialized = initialized;
        self
    }
}
