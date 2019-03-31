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
