use super::value::Value;
use crate::vm::vm::Factory;

pub const SYMBOL_ITERATOR_ID: usize = usize::MAX - 1;
pub const SYMBOL_TO_PRIMITIVE_ID: usize = usize::MAX - 2;
pub const SYMBOL_TO_STRING_TAG_ID: usize = usize::MAX - 3;
pub const SYMBOL_ASYNC_ITERATOR_ID: usize = usize::MAX - 4;
pub const SYMBOL_HAS_INSTANCE_ID: usize = usize::MAX - 5;
pub const SYMBOL_IS_CONCAT_SPREADABLE_ID: usize = usize::MAX - 6;
pub const SYMBOL_MATCH_ID: usize = usize::MAX - 7;
pub const SYMBOL_MATCH_ALL_ID: usize = usize::MAX - 8;
pub const SYMBOL_REPLACE_ID: usize = usize::MAX - 9;
pub const SYMBOL_SEARCH_ID: usize = usize::MAX - 10;
pub const SYMBOL_SPECIES_ID: usize = usize::MAX - 11;
pub const SYMBOL_SPLIT_ID: usize = usize::MAX - 12;
pub const SYMBOL_UNSCOPABLES_ID: usize = usize::MAX - 13;

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub id: usize,
    pub description: Option<String>,
    pub registered: bool,
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
        sym.get_symbol_info().registered = true;
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
