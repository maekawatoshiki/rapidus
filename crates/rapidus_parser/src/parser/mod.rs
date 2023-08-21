use rapidus_ast::{
    expr::Expr,
    literal::Num,
    module::{Module, ModuleItem},
    span::Span,
    stmt::{self, Stmt},
};

use crate::{
    error::Error,
    lexer::Lexer,
    t,
    token::{num::Num as TokNum, op::Op, Token},
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

        while let Some((span, tok)) = self.read_spanned_token()? {
            let item = match tok {
                t!(";") => ModuleItem::Stmt(Stmt::new(span, stmt::Kind::Empty)),
                _ => ModuleItem::Stmt(self.parse_expr_stmt(span, tok)?),
            };
            children.push(item);
        }

        Ok(children)
    }

    fn parse_expr_stmt(&mut self, span: Span, tok: Token) -> Result<Stmt, Error> {
        self.parse_primary_expr(span, tok)
            .map(|expr| Stmt::new(span, stmt::Kind::Expr(expr)))
    }

    fn parse_primary_expr(&mut self, span: Span, tok: Token) -> Result<Expr, Error> {
        match tok {
            Token::Num(TokNum { val, raw }) => Ok(Expr::Literal(Num::new(span, val, raw).into())),
            _ => Err(Error::Todo),
        }
    }

    fn read_spanned_token(&mut self) -> Result<Option<(Span, Token)>, Error> {
        let start = self.lexer.cur_pos();
        let tok = self.lexer.read()?;
        let end = self.lexer.cur_pos();
        Ok(tok.map(|tok| (Span::new(start, end), tok)))
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
    fn parse_num() {
        let source = Source::new(SourceName::FileName("test.js".into()), r#"123"#);
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }
}
