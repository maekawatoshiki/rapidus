use super::{super::super::gc::MemoryAllocator, value::Value};

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
    pub list: Vec<(String, Value)>,
}

impl GlobalSymbolRegistry {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn key_for(&mut self, allocator: &mut MemoryAllocator, sym: Value) -> Value {
        if let Some((key, _)) = self.list.iter().find(|(_, sym_)| sym == *sym_) {
            return Value::string(allocator, key.to_owned());
        }

        Value::undefined()
    }
}
