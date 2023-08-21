use crate::span::Span;

/// Represents a module.
/// https://tc39.es/ecma262/multipage/ecmascript-language-scripts-and-modules.html#prod-Module
#[derive(Debug, Clone)]
pub struct Module {
    span: Span,
}

impl Module {
    pub const fn span(&self) -> &Span {
        &self.span
    }
}
