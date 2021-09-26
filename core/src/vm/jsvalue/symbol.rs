use super::value::Value;
use crate::vm::vm::Factory;

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub id: usize,
    pub description: Option<String>,
}

impl SymbolInfo {
    pub fn get_description_string<'a>(&'a self) -> &'a str {
        match self.description {
            Some(ref s) => s.as_str(),
            None => "",
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalSymbolRegistry {
    list: Vec<(String, Value)>,
}

impl GlobalSymbolRegistry {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn for_(&mut self, factory: &mut Factory, key: String) -> Value {
        if let Some((_, sym)) = self.list.iter().find(|(key_, _)| key == *key_) {
            return *sym;
        }

        let sym = factory.symbol(Some(key.clone()));
        self.list.push((key, sym));

        sym
    }

    pub fn key_for(&mut self, factory: &mut Factory, sym: Value) -> Value {
        if let Some((key, _)) = self.list.iter().find(|(_, sym_)| sym == *sym_) {
            return factory.string(key.to_owned());
        }

        Value::undefined()
    }
}
