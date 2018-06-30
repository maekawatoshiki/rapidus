use lexer;
use token::{Keyword, Kind, Symbol};
use node::Node;

#[derive(Clone, Debug)]
pub struct Parser {
    pub lexer: lexer::Lexer,
}

impl Parser {
    pub fn new(code: String) -> Parser {
        Parser {
            lexer: lexer::Lexer::new(code),
        }
    }
}

impl Parser {
    pub fn next(&mut self) -> Result<Node, ()> {
        self.read_script()
    }
}

impl Parser {
    fn read_script(&mut self) -> Result<Node, ()> {
        self.read_statement_list()
    }
}

impl Parser {
    fn read_statement_list(&mut self) -> Result<Node, ()> {
        let mut items = vec![];

        loop {
            if self.lexer.eof() {
                if items.is_empty() {
                    return Err(());
                }
                break;
            }

            if let Ok(ok) = self.lexer.peek() {
                if ok.is_the_symbol(Symbol::ClosingBrace) {
                    break;
                }
            }

            if let Ok(item) = self.read_statement_list_item() {
                items.push(item)
            }
        }

        Ok(Node::StatementList(items))
    }

    fn read_statement_list_item(&mut self) -> Result<Node, ()> {
        if self.is_declaration() {
            self.read_declaration()
        } else {
            self.read_statement()
        }
    }

    fn read_statement(&mut self) -> Result<Node, ()> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Keyword(Keyword::If) => self.read_if_statement(),
            _ => {
                self.lexer.unget(&tok);
                self.read_expression_statement()
            }
        }
    }
}

impl Parser {
    fn read_if_statement(&mut self) -> Result<Node, ()> {
        assert_eq!(self.lexer.next()?.kind, Kind::Symbol(Symbol::OpeningParen));
        let cond = self.read_expression()?;
        assert_eq!(self.lexer.next()?.kind, Kind::Symbol(Symbol::ClosingParen));

        let then_ = self.read_statement()?;

        if let Ok(expect_else_tok) = self.lexer.next() {
            if expect_else_tok.kind == Kind::Keyword(Keyword::Else) {
                let else_ = self.read_statement()?;
                return Ok(Node::If(Box::new(cond), Box::new(then_), Box::new(else_)));
            } else {
                self.lexer.unget(&expect_else_tok);
            }
        }

        Ok(Node::If(
            Box::new(cond),
            Box::new(then_),
            Box::new(Node::StatementList(vec![])),
        ))
    }
}

macro_rules! expression { ( $name:ident, $lower:ident, [ $( $op:path ),* ] ) => {
    fn $name (&mut self) -> Result<Node, ()> {
        let mut lhs = self. $lower ()?;
        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(ref op) if $( op == &$op )||* => {
                    lhs = Node::BinOp(
                        Box::new(lhs),
                        Box::new(self. $name ()?),
                        op.as_binop().unwrap(),
                    )
                }
                _ => self.lexer.unget(&tok),
            }
        }
        Ok(lhs)
    }
} }

impl Parser {
    fn read_expression_statement(&mut self) -> Result<Node, ()> {
        self.read_expression()
    }

    fn read_expression(&mut self) -> Result<Node, ()> {
        let lhs = self.read_assignment_expression();
        lhs
    }

    /// https://tc39.github.io/ecma262/#prod-AssignmentExpression
    fn read_assignment_expression(&mut self) -> Result<Node, ()> {
        let lhs = self.read_equality_expression();
        lhs
    }

    /// https://tc39.github.io/ecma262/#prod-EqualityExpression
    expression!(
        read_equality_expression,
        read_relational_expression,
        [Symbol::Eq, Symbol::Ne, Symbol::SEq, Symbol::SNe]
    );

    /// https://tc39.github.io/ecma262/#prod-RelationalExpression
    expression!(
        read_relational_expression,
        read_additive_expression,
        [Symbol::Lt, Symbol::Gt, Symbol::Le, Symbol::Ge]
    );

    /// https://tc39.github.io/ecma262/#prod-AdditiveExpression
    expression!(
        read_additive_expression,
        read_multiplicate_expression,
        [Symbol::Add, Symbol::Sub]
    );

    /// https://tc39.github.io/ecma262/#prod-MultiplicativeExpression
    expression!(
        read_multiplicate_expression,
        read_primary_expression,
        [Symbol::Asterisk, Symbol::Div]
    );

    /// https://tc39.github.io/ecma262/#prod-PrimaryExpression
    fn read_primary_expression(&mut self) -> Result<Node, ()> {
        match self.lexer.next()?.kind {
            Kind::Keyword(Keyword::This) => unimplemented!(),
            Kind::Identifier(ref i) if i == "true" => Ok(Node::Boolean(true)),
            Kind::Identifier(ref i) if i == "false" => Ok(Node::Boolean(false)),
            Kind::Identifier(ident) => Ok(Node::Identifier(ident)),
            Kind::Number(num) => Ok(Node::Number(num)),
            Kind::LineTerminator => self.read_primary_expression(),
            e => unimplemented!("{:?}", e),
        }
    }
}

impl Parser {
    fn is_declaration(&mut self) -> bool {
        self.is_hoistable_declaration()
    }

    fn read_declaration(&mut self) -> Result<Node, ()> {
        Err(())
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-HoistableDeclaration
    fn is_hoistable_declaration(&mut self) -> bool {
        self.is_function_declaration()
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn is_function_declaration(&mut self) -> bool {
        match self.lexer.peek() {
            Ok(tok) => tok.is_the_keyword(Keyword::Function),
            Err(_) => false,
        }
    }
}

#[test]
fn number() {
    let mut parser = Parser::new("12345".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Number(12345.0)])
    );
}

#[test]
fn boolean() {
    let mut parser = Parser::new("true".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Boolean(true)])
    );
}

#[test]
fn identifier() {
    let mut parser = Parser::new("variable".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Identifier("variable".to_string())])
    );
}

#[test]
fn simple_expr1() {
    use node::BinOp;

    let mut parser = Parser::new("31 + 26 / 3 - 1 * 20".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::BinOp(
                Box::new(Node::Number(31.0)),
                Box::new(Node::BinOp(
                    Box::new(Node::BinOp(
                        Box::new(Node::Number(26.0)),
                        Box::new(Node::Number(3.0)),
                        BinOp::Div,
                    )),
                    Box::new(Node::BinOp(
                        Box::new(Node::Number(1.0)),
                        Box::new(Node::Number(20.0)),
                        BinOp::Mul,
                    )),
                    BinOp::Sub,
                )),
                BinOp::Add,
            ),
        ])
    );
}

#[test]
fn simple_expr2() {
    use node::BinOp;

    for (input, op) in [
        ("1 + 2 == 3", BinOp::Eq),
        ("1 + 2 != 3", BinOp::Ne),
        ("1 + 2 === 3", BinOp::SEq),
        ("1 + 2 !== 3", BinOp::SNe),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![
                Node::BinOp(
                    Box::new(Node::BinOp(
                        Box::new(Node::Number(1.0)),
                        Box::new(Node::Number(2.0)),
                        BinOp::Add,
                    )),
                    Box::new(Node::Number(3.0)),
                    op.clone(),
                ),
            ])
        );
    }
}

#[test]
fn if_() {
    use node::BinOp;

    let mut parser = Parser::new("if (x <= 2) then_stmt else else_stmt".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::If(
                Box::new(Node::BinOp(
                    Box::new(Node::Identifier("x".to_string())),
                    Box::new(Node::Number(2.0)),
                    BinOp::Le,
                )),
                Box::new(Node::Identifier("then_stmt".to_string())),
                Box::new(Node::Identifier("else_stmt".to_string())),
            ),
        ])
    );

    parser = Parser::new("if (x <= 2) then_stmt ".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::If(
                Box::new(Node::BinOp(
                    Box::new(Node::Identifier("x".to_string())),
                    Box::new(Node::Number(2.0)),
                    BinOp::Le,
                )),
                Box::new(Node::Identifier("then_stmt".to_string())),
                Box::new(Node::StatementList(vec![])),
            ),
        ])
    );
}
