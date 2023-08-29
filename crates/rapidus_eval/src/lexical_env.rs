use ecow::EcoString;
use rustc_hash::FxHashMap;

use crate::value::JsValue;

#[derive(Debug, Clone)]
pub enum LexicalEnv {
    Module(ModuleEnvRecord),
}

#[derive(Debug, Clone)]
pub struct ModuleEnvRecord {
    bindings: FxHashMap<EcoString, JsValue>,
}

pub trait EnvRecord {
    fn bindings(&mut self) -> &FxHashMap<EcoString, JsValue>;
    fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, JsValue>;

    fn create_mutable_binding(&mut self, name: impl Into<EcoString>) {
        self.bindings_mut()
            .insert(name.into(), JsValue::undefined());
    }

    fn initialize_binding(&mut self, name: impl Into<EcoString>, value: JsValue) {
        self.bindings_mut().insert(name.into(), value);
    }
}

impl EnvRecord for LexicalEnv {
    fn bindings(&mut self) -> &FxHashMap<EcoString, JsValue> {
        match self {
            Self::Module(record) => record.bindings(),
        }
    }

    fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, JsValue> {
        match self {
            Self::Module(record) => record.bindings_mut(),
        }
    }
}

impl ModuleEnvRecord {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }
}

impl EnvRecord for ModuleEnvRecord {
    fn bindings(&mut self) -> &FxHashMap<EcoString, JsValue> {
        &self.bindings
    }

    fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, JsValue> {
        &mut self.bindings
    }
}
