use rapidus_ast::{
    expr::Expr,
    module::{Module, ModuleItem},
    span::Span,
    stmt::{self, Stmt},
};

use crate::{
    error::Error,
    lexer::Lexer,
    t,
    token::{op::Op, Token},
};

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
        // TODO: https://tc39.es/ecma262/multipage/ecmascript-language-scripts-and-modules.html#prod-ModuleItem
        let start = self.lexer.cur_pos();
        self.parse_stmt_list()
            .map(|children| Module::new(Span::new(start, self.lexer.cur_pos()), children))
    }

    fn parse_stmt_list(&mut self) -> Result<Vec<ModuleItem>, Error> {
        // TODO: https://tc39.es/ecma262/multipage/ecmascript-language-statements-and-declarations.html#prod-StatementListItem
        let mut children = vec![];
        let mut start = self.lexer.cur_pos();

        while let Some(tok) = self.lexer.read()? {
            let item = match tok {
                t!(";") => ModuleItem::Stmt(Stmt::new(
                    Span::new(start, self.lexer.cur_pos()),
                    stmt::Kind::Empty,
                )),
                _ => ModuleItem::Stmt(self.parse_expr_stmt(tok, start)?),
            };
            children.push(item);
            start = self.lexer.cur_pos();
        }

        Ok(children)
    }

    fn parse_expr_stmt(&mut self, _tok: Token, start: usize) -> Result<Stmt, Error> {
        self.parse_primary_expr().map(|expr| {
            Stmt::new(
                Span::new(start, self.lexer.cur_pos()),
                stmt::Kind::Expr(expr),
            )
        })
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, Error> {
        Err(Error::Todo)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Input,
        source::{Source, SourceName},
    };

    use super::*;

    #[test]
    fn parse_empty() {
        let source = Source::new(SourceName::FileName("test.js".into()), r#";"#);
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    #[should_panic]
    fn parse_num() {
        let source = Source::new(SourceName::FileName("test.js".into()), r#"123"#);
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }
}
