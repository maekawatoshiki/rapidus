use super::{super::super::gc::MemoryAllocator, prototype::ObjectPrototypes, value::Value2};

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
    list: Vec<(String, Value2)>,
}

impl GlobalSymbolRegistry {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn for_(
        &mut self,
        allocator: &mut MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        key: String,
    ) -> Value2 {
        if let Some((_, sym)) = self.list.iter().find(|(key_, _)| key == *key_) {
            return *sym;
        }

        let sym = Value2::symbol(allocator, object_prototypes, Some(key.clone()));
        self.list.push((key, sym));

        sym
    }
}
