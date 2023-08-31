use std::collections::VecDeque;

use rapidus_ast::{
    bin::{BinOp, BinOpExpr},
    decl::{self, Decl, LexicalDecl},
    expr::Expr,
    ident::Ident as Ident_,
    literal::{Null, Num, Str as Str_},
    module::{Module, ModuleItem},
    span::{Span, Spanned},
    stmt::{self, Stmt},
    Node as _,
};

use crate::{
    error::{Error, SyntaxError},
    lexer::Lexer,
    t,
    token::{
        ident::{Ident, ReservedWord},
        num::Num as TokNum,
        op::{AssignOp, Op},
        str::Str,
        Token,
    },
};

macro_rules! parse_binop {
    ($name:ident, $child:ident, ($($op:tt),*)) => {
        fn $name(&mut self) -> Result<Expr, Error> {
            let mut lhs = self.$child()?;
            let mut buf = VecDeque::new();
            while let Some(Spanned(span, tok)) = self.lexer.read()? {
                match tok {
                    $(t!($op))|* => {
                        let rhs = self.$child()?;
                        lhs = Expr::BinOp(BinOpExpr::new(
                            Span::new(lhs.span().start(), rhs.span().end()),
                            BinOp::try_from(tok).unwrap(),
                            lhs,
                            rhs,
                        ));
                        buf.clear();
                    }
                    Token::LineTerminator(_) => buf.push_back(Spanned(span, tok)),
                    _ => {
                        buf.push_back(Spanned(span, tok));
                        break;
                    }
                }
            }
            while let Some(tok) = buf.pop_front() {
                self.lexer.unread(tok);
            }
            Ok(lhs)
        }
    };
}

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
                Token::Ident(Ident::Ident(name)) if name == "let" => {
                    ModuleItem::Decl(self.parse_lexical_decl()?)
                }
                t!(";") => ModuleItem::Stmt(Stmt::new(span, stmt::Kind::Empty)),
                Token::LineTerminator(_) => continue,
                _ => {
                    self.lexer.unread(Spanned(span, tok));
                    ModuleItem::Stmt(self.parse_expr_stmt()?)
                }
            };
            children.push(item);
        }

        Ok(children)
    }

    fn parse_lexical_decl(&mut self) -> Result<Decl, Error> {
        let binding = self
            .lexer
            .read()?
            .ok_or(Error::SyntaxError(SyntaxError::UnexpectedEndOfInput))?;
        let (bspan, binding) = match binding {
            Spanned(bspan, Token::Ident(Ident::Ident(ident))) => (bspan, ident),
            _ => return Err(Error::SyntaxError(SyntaxError::UnexpectedToken(binding))),
        };
        let init = self
            .expect_token(t!("="))
            .is_ok()
            .then(|| self.parse_expr())
            .transpose()?;
        let sspan = self.expect_semicolon()?;
        Ok(Decl::new(
            Span::new(bspan.start(), sspan.end()),
            decl::Kind::LexicalDecl(LexicalDecl::new(
                Span::new(
                    bspan.start(),
                    init.as_ref().map_or(bspan.end(), |i| i.span().end()),
                ),
                binding,
                init,
            )),
        ))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.parse_expr()?;
        self.expect_semicolon()?;
        Ok(Stmt::new(expr.span(), stmt::Kind::Expr(expr)))
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_additive_expr()
    }

    parse_binop!(parse_additive_expr, parse_multiplicative_expr, ("+", "-"));
    parse_binop!(
        parse_multiplicative_expr,
        parse_primary_expr,
        ("*", "/", "%")
    );

    fn parse_primary_expr(&mut self) -> Result<Expr, Error> {
        let Spanned(span, tok) = self
            .lexer
            .read()?
            .ok_or(Error::SyntaxError(SyntaxError::UnexpectedEndOfInput))?;
        match tok {
            Token::Num(TokNum { val, raw }) => Ok(Expr::Literal(Num::new(span, val, raw).into())),
            Token::Ident(Ident::Reserved(ReservedWord::Null)) => {
                Ok(Expr::Literal(Null::new(span).into()))
            }
            Token::Ident(Ident::Ident(i)) => Ok(Expr::Ident(Ident_::new(span, i))),
            Token::Str(Str { val, raw }) => Ok(Expr::Literal(Str_::new(span, val, raw).into())),
            t!("(") => {
                let expr = self.parse_expr()?;
                self.expect_rparen()?;
                Ok(expr)
            }
            Token::LineTerminator(_) => self.parse_primary_expr(),
            _ => Err(Error::Todo(span)),
        }
    }

    fn expect_semicolon(&mut self) -> Result<Span, Error> {
        // https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-rules-of-automatic-semicolon-insertion

        let tok = self.lexer.read()?;
        match tok {
            Some(Spanned(span, t!(";"))) => Ok(span),
            Some(Spanned(span, t!(")"))) => {
                self.lexer.unread(tok.unwrap());
                Ok(span)
            }
            Some(Spanned(span, Token::LineTerminator(_))) => Ok(span),
            None => Ok(Span::new(
                self.lexer.source().len() - 1,
                self.lexer.source().len(),
            )),
            // TODO: For now, handle this case as an unrecoverable error.
            Some(tok) => Err(Error::SyntaxError(SyntaxError::UnexpectedToken(tok))),
        }
    }

    fn expect_token(&mut self, token: Token) -> Result<Span, Error> {
        let tok = self.lexer.read()?;
        match tok {
            Some(Spanned(span, t)) if t == token => Ok(span),
            Some(Spanned(_, Token::LineTerminator(_))) => self.expect_token(token),
            // TODO: For now, handle this case as an unrecoverable error.
            Some(tok) => Err(Error::SyntaxError(SyntaxError::UnexpectedToken(tok))),
            None => Err(Error::SyntaxError(SyntaxError::UnexpectedEndOfInput)),
        }
    }

    fn expect_rparen(&mut self) -> Result<(), Error> {
        let tok = self.lexer.read()?;
        match tok {
            Some(Spanned(_, t!(")"))) => Ok(()),
            Some(Spanned(_, Token::LineTerminator(_))) => self.expect_rparen(),
            // TODO: For now, handle this case as an unrecoverable error.
            Some(tok) => Err(Error::SyntaxError(SyntaxError::UnexpectedToken(tok))),
            None => Err(Error::SyntaxError(SyntaxError::UnexpectedEndOfInput)),
        }
    }
}

impl TryFrom<Token> for BinOp {
    type Error = ();
    fn try_from(tok: Token) -> Result<Self, Self::Error> {
        match tok {
            t!("+") => Ok(BinOp::Add),
            t!("-") => Ok(BinOp::Sub),
            t!("*") => Ok(BinOp::Mul),
            t!("/") => Ok(BinOp::Div),
            t!("%") => Ok(BinOp::Mod),
            _ => Err(()),
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
            Error::SyntaxError(_)
        ));
    }

    #[test]
    fn parse_paren_num() {
        let source = Source::new(SourceName::FileName("test.js".into()), "(42\n)\n(1)");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_null() {
        let source = Source::new(SourceName::FileName("test.js".into()), "null");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_ident() {
        let source = Source::new(SourceName::FileName("test.js".into()), "foo; undefined");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_string() {
        let source = Source::new(SourceName::FileName("test.js".into()), r#""foo""#);
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_add_1() {
        let source = Source::new(SourceName::FileName("test.js".into()), "2 + 40");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_add_2() {
        let source = Source::new(SourceName::FileName("test.js".into()), "2 \n+ 40");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_add_3() {
        let source = Source::new(SourceName::FileName("test.js".into()), "2 \n+ \n40");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }

    #[test]
    fn parse_sub() {
        let source = Source::new(SourceName::FileName("test.js".into()), "2 - 42");
        let lexer = Lexer::new(Input::from(&source));
        let module = Parser::new(lexer).parse_module().unwrap();
        insta::assert_debug_snapshot!(module);
    }
}
