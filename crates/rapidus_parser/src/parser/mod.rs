use rapidus_ast::{
    expr::Expr,
    literal::Num,
    module::{Module, ModuleItem},
    span::{Span, Spanned},
    stmt::{self, Stmt},
    Node as _,
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

        while let Some(Spanned(span, tok)) = self.lexer.read()? {
            let item = match tok {
                t!(";") => ModuleItem::Stmt(Stmt::new(span, stmt::Kind::Empty)),
                _ => {
                    self.lexer.unread(Spanned(span, tok));
                    ModuleItem::Stmt(self.parse_expr_stmt()?)
                }
            };
            children.push(item);
        }

        Ok(children)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.parse_expr()?;
        self.expect_semicolon()?;
        Ok(Stmt::new(expr.span(), stmt::Kind::Expr(expr)))
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, Error> {
        let Spanned(span, tok) = self.lexer.read()?.unwrap();
        match tok {
            Token::Num(TokNum { val, raw }) => Ok(Expr::Literal(Num::new(span, val, raw).into())),
            Token::LParen => {
                let expr = self.parse_expr()?;
                self.expect_rparen()?;
                Ok(expr)
            }
            _ => Err(Error::Todo),
        }
    }

    fn expect_semicolon(&mut self) -> Result<(), Error> {
        // https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-rules-of-automatic-semicolon-insertion

        let tok = self.lexer.read()?;
        match tok {
            Some(Spanned(_, Token::Op(Op::Semicolon))) => Ok(()),
            Some(Spanned(_, Token::RBrace)) => {
                self.lexer.unread(tok.unwrap());
                Ok(())
            }
            Some(Spanned(_, Token::LineTerminator(_))) => Ok(()),
            None => Ok(()),
            // TODO: For now, handle this case as an unrecoverable error.
            _ => Err(Error::SyntaxError),
        }
    }

    fn expect_rparen(&mut self) -> Result<(), Error> {
        let tok = self.lexer.read()?;
        match tok {
            Some(Spanned(_, Token::RParen)) => Ok(()),
            Some(Spanned(_, Token::LineTerminator(_))) => self.expect_rparen(),
            // TODO: For now, handle this case as an unrecoverable error.
            _ => Err(Error::SyntaxError),
        }
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

    #[test]
    fn parse_nums() {
        let source = Source::new(SourceName::FileName("test.js".into()), "123; 345; 1\n2");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_nums_error() {
        let source = Source::new(SourceName::FileName("test.js".into()), r#"123 345"#);
        let lexer = Lexer::new(Input::from(&source));
        assert!(matches!(
            Parser::new(lexer).parse_module().unwrap_err(),
            Error::SyntaxError
        ));
    }

    #[test]
    fn parse_paren_num() {
        let source = Source::new(SourceName::FileName("test.js".into()), "(42\n)\n(1)");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }
}
