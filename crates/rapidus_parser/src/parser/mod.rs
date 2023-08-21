use rapidus_ast::module::Module;

use crate::{error::Error, lexer::Lexer};

/// Parser.
pub struct Parser<'a> {
    /// Lexical analyzer used in the parser.
    #[allow(dead_code)]
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse_module(&mut self) -> Result<Module, Error> {
        Err(Error::Todo)
    }
}
