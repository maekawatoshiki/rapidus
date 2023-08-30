use ecow::EcoString;
use rustc_hash::FxHashMap;

use crate::value::JsValue;

#[derive(Debug, Clone)]
pub enum Environment {
    Module(ModuleEnv),
    // Lexical(LexicalEnv),
    // Global(GlobalEnv),
    // Object(ObjectEnv),
    // Function(FunctionEnv),
}

#[derive(Debug, Clone)]
pub struct ModuleEnv {
    bindings: FxHashMap<EcoString, JsValue>,
}

pub trait EnvRecord {
    fn bindings(&self) -> &FxHashMap<EcoString, JsValue>;
    fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, JsValue>;

    fn create_mutable_binding(&mut self, name: impl Into<EcoString>) {
        self.bindings_mut()
            .insert(name.into(), JsValue::undefined());
    }

    fn initialize_binding(&mut self, name: impl Into<EcoString>, value: JsValue) {
        self.bindings_mut().insert(name.into(), value);
    }

    fn get_binding_value(&self, name: impl AsRef<str>) -> Option<JsValue> {
        self.bindings().get(name.as_ref()).copied()
    }
}

impl EnvRecord for Environment {
    fn bindings(&self) -> &FxHashMap<EcoString, JsValue> {
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

impl ModuleEnv {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }
}

impl EnvRecord for ModuleEnv {
    fn bindings(&self) -> &FxHashMap<EcoString, JsValue> {
        &self.bindings
    }

    fn bindings_mut(&mut self) -> &mut FxHashMap<EcoString, JsValue> {
        &mut self.bindings
    }
}
