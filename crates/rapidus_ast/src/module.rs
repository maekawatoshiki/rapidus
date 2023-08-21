use crate::{decl::Decl, span::Span, stmt::Stmt, Node};

/// Represents a module.
/// https://tc39.es/ecma262/multipage/ecmascript-language-scripts-and-modules.html#prod-Module
#[derive(Debug, Clone)]
pub struct Module {
    span: Span,
    children: Vec<ModuleItem>,
}

/// https://tc39.es/ecma262/multipage/ecmascript-language-scripts-and-modules.html#prod-ModuleItem
#[derive(Debug, Clone)]
pub enum ModuleItem {
    Stmt(Stmt),
    Decl(Decl),
}

impl Module {
    pub const fn new(span: Span, children: Vec<ModuleItem>) -> Self {
        Self { span, children }
    }

    pub fn children(&self) -> &[ModuleItem] {
        &self.children
    }
}

impl Node for Module {
    fn span(&self) -> Span {
        self.span
    }
}
