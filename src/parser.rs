use lexer;
use node::{BinOp, Node, UnaryOp};
use token::{Keyword, Kind, Symbol};

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
        let lhs = self. $lower ()?;
        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(ref op) if $( op == &$op )||* => {
                    return Ok(Node::BinaryOp(
                        Box::new(lhs),
                        Box::new(self. $name ()?),
                        op.as_binop().unwrap(),
                    ));
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
    // TODO: Implement all features.
    fn read_assignment_expression(&mut self) -> Result<Node, ()> {
        let mut lhs = self.read_conditional_expression()?;
        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(Symbol::Assign) => {
                    lhs = Node::Assign(Box::new(lhs), Box::new(self.read_assignment_expression()?))
                }
                _ => self.lexer.unget(&tok),
            }
        }
        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-ConditionalExpression
    fn read_conditional_expression(&mut self) -> Result<Node, ()> {
        let lhs = self.read_logical_or_expression()?;
        if let Ok(tok) = self.lexer.next() {
            if let Kind::Symbol(Symbol::Question) = tok.kind {
                let then_ = self.read_conditional_expression()?;
                assert_eq!(self.lexer.next()?.kind, Kind::Symbol(Symbol::Colon));
                let else_ = self.read_conditional_expression()?;
                return Ok(Node::TernaryOp(
                    Box::new(lhs),
                    Box::new(then_),
                    Box::new(else_),
                ));
            } else {
                self.lexer.unget(&tok);
            }
        }
        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-LogicalORExpression
    expression!(
        read_logical_or_expression,
        read_logical_and_expression,
        [Symbol::LOr]
    );

    /// https://tc39.github.io/ecma262/#prod-LogicalANDExpression
    expression!(
        read_logical_and_expression,
        read_bitwise_or_expression,
        [Symbol::LAnd]
    );

    /// https://tc39.github.io/ecma262/#prod-BitwiseORExpression
    expression!(
        read_bitwise_or_expression,
        read_bitwise_xor_expression,
        [Symbol::Or]
    );

    /// https://tc39.github.io/ecma262/#prod-BitwiseXORExpression
    expression!(
        read_bitwise_xor_expression,
        read_bitwise_and_expression,
        [Symbol::Xor]
    );

    /// https://tc39.github.io/ecma262/#prod-BitwiseANDExpression
    expression!(
        read_bitwise_and_expression,
        read_equality_expression,
        [Symbol::And]
    );

    /// https://tc39.github.io/ecma262/#prod-EqualityExpression
    expression!(
        read_equality_expression,
        read_relational_expression,
        [Symbol::Eq, Symbol::Ne, Symbol::SEq, Symbol::SNe]
    );

    /// https://tc39.github.io/ecma262/#prod-RelationalExpression
    expression!(
        read_relational_expression,
        read_shift_expression,
        [Symbol::Lt, Symbol::Gt, Symbol::Le, Symbol::Ge]
    );

    /// https://tc39.github.io/ecma262/#prod-ShiftExpression
    expression!(
        read_shift_expression,
        read_additive_expression,
        [Symbol::Shl, Symbol::Shr, Symbol::ZFShr]
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
        read_exponentiation_expression,
        [Symbol::Asterisk, Symbol::Div, Symbol::Mod]
    );

    /// https://tc39.github.io/ecma262/#prod-ExponentiationExpression
    fn read_exponentiation_expression(&mut self) -> Result<Node, ()> {
        if self.is_unary_expression() {
            return self.read_unary_expression();
        }
        let lhs = self.read_update_expression()?;
        if let Ok(tok) = self.lexer.next() {
            if let Kind::Symbol(Symbol::Exp) = tok.kind {
                return Ok(Node::BinaryOp(
                    Box::new(lhs),
                    Box::new(self.read_exponentiation_expression()?),
                    BinOp::Exp,
                ));
            } else {
                self.lexer.unget(&tok);
            }
        }
        Ok(lhs)
    }

    fn is_unary_expression(&mut self) -> bool {
        match self.lexer.peek() {
            Ok(ok) => match ok.kind {
                Kind::Keyword(Keyword::Delete)
                | Kind::Keyword(Keyword::Void)
                | Kind::Keyword(Keyword::Typeof)
                | Kind::Symbol(Symbol::Add)
                | Kind::Symbol(Symbol::Sub)
                | Kind::Symbol(Symbol::BitwiseNot)
                | Kind::Symbol(Symbol::Not) => true,
                _ => false,
            },
            Err(_) => false,
        }
    }

    /// https://tc39.github.io/ecma262/#prod-UnaryExpression
    fn read_unary_expression(&mut self) -> Result<Node, ()> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Keyword(Keyword::Delete) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::Delete,
            )),
            Kind::Keyword(Keyword::Void) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::Void,
            )),
            Kind::Keyword(Keyword::Typeof) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::Typeof,
            )),
            Kind::Symbol(Symbol::Add) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::Plus,
            )),
            Kind::Symbol(Symbol::Sub) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::Minus,
            )),
            Kind::Symbol(Symbol::BitwiseNot) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::BitwiseNot,
            )),
            Kind::Symbol(Symbol::Not) => Ok(Node::UnaryOp(
                Box::new(self.read_unary_expression()?),
                UnaryOp::Not,
            )),
            _ => {
                self.lexer.unget(&tok);
                self.read_update_expression()
            }
        }
    }

    /// https://tc39.github.io/ecma262/#prod-UpdateExpression
    // TODO: Implement all features.
    fn read_update_expression(&mut self) -> Result<Node, ()> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Symbol(Symbol::Inc) => {
                return Ok(Node::UnaryOp(
                    Box::new(self.read_left_hand_side_expression()?),
                    UnaryOp::PrInc,
                ))
            }
            Kind::Symbol(Symbol::Dec) => {
                return Ok(Node::UnaryOp(
                    Box::new(self.read_left_hand_side_expression()?),
                    UnaryOp::PrDec,
                ))
            }
            _ => self.lexer.unget(&tok),
        }

        let e = self.read_left_hand_side_expression()?;
        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(Symbol::Inc) => return Ok(Node::UnaryOp(Box::new(e), UnaryOp::PoInc)),
                Kind::Symbol(Symbol::Dec) => return Ok(Node::UnaryOp(Box::new(e), UnaryOp::PoDec)),
                _ => self.lexer.unget(&tok),
            }
        }

        Ok(e)
    }

    /// https://tc39.github.io/ecma262/#prod-LeftHandSideExpression
    // TODO: Implement all features.
    fn read_left_hand_side_expression(&mut self) -> Result<Node, ()> {
        let lhs = self.read_call_expression()?;

        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-CallExpression
    // TODO: Implement all features.
    fn read_call_expression(&mut self) -> Result<Node, ()> {
        let mut lhs = self.read_primary_expression()?;

        while let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(Symbol::OpeningParen) => {
                    let args = self.read_arguments()?;
                    lhs = Node::Call(Box::new(lhs), args)
                }
                Kind::Symbol(Symbol::Point) => match self.lexer.next()?.kind {
                    Kind::Identifier(name) => lhs = Node::Member(Box::new(lhs), name),
                    _ => panic!("error: expected identifier"),
                },
                _ => {
                    self.lexer.unget(&tok);
                    break;
                }
            }
        }

        Ok(lhs)
    }

    fn read_arguments(&mut self) -> Result<Vec<Node>, ()> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Symbol(Symbol::ClosingParen) => return Ok(vec![]),
            _ => {
                self.lexer.unget(&tok);
            }
        }

        let mut args = vec![];
        loop {
            match self.lexer.next() {
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::ClosingParen) => break,
                Ok(tok) => self.lexer.unget(&tok),
                Err(_) => break,
            }

            if let Ok(arg) = self.read_assignment_expression() {
                args.push(arg)
            }

            match self.lexer.next() {
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::Comma) => {}
                Ok(tok) => self.lexer.unget(&tok),
                _ => break,
            }
        }

        Ok(args)
    }

    /// https://tc39.github.io/ecma262/#prod-PrimaryExpression
    fn read_primary_expression(&mut self) -> Result<Node, ()> {
        match self.lexer.next()?.kind {
            Kind::Keyword(Keyword::This) => unimplemented!(),
            Kind::Identifier(ref i) if i == "true" => Ok(Node::Boolean(true)),
            Kind::Identifier(ref i) if i == "false" => Ok(Node::Boolean(false)),
            Kind::Identifier(ident) => Ok(Node::Identifier(ident)),
            Kind::String(s) => Ok(Node::String(s)),
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
fn simple_expr_5arith() {
    use node::BinOp;

    let mut parser = Parser::new("31 + 26 / 3 - 1 * 20 % 3".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::BinaryOp(
                Box::new(Node::Number(31.0)),
                Box::new(Node::BinaryOp(
                    Box::new(Node::BinaryOp(
                        Box::new(Node::Number(26.0)),
                        Box::new(Node::Number(3.0)),
                        BinOp::Div,
                    )),
                    Box::new(Node::BinaryOp(
                        Box::new(Node::Number(1.0)),
                        Box::new(Node::BinaryOp(
                            Box::new(Node::Number(20.0)),
                            Box::new(Node::Number(3.0)),
                            BinOp::Rem,
                        )),
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
fn simple_expr_eq() {
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
                Node::BinaryOp(
                    Box::new(Node::BinaryOp(
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
fn simple_expr_cond() {
    use node::BinOp;

    let mut parser = Parser::new("n == 1 ? 2 : max".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::TernaryOp(
                Box::new(Node::BinaryOp(
                    Box::new(Node::Identifier("n".to_string())),
                    Box::new(Node::Number(1.0)),
                    BinOp::Eq,
                )),
                Box::new(Node::Number(2.0)),
                Box::new(Node::Identifier("max".to_string())),
            ),
        ])
    );
}

#[test]
fn simple_expr_logical_or() {
    use node::BinOp;

    for (input, op) in [("1 || 0", BinOp::LOr), ("1 && 0", BinOp::LAnd)].iter() {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![
                Node::BinaryOp(
                    Box::new(Node::Number(1.0)),
                    Box::new(Node::Number(0.0)),
                    op.clone(),
                ),
            ])
        );
    }
}

#[test]
fn simple_expr_bitwise_and() {
    use node::BinOp;

    for (input, op) in [
        ("1 & 3", BinOp::And),
        ("1 ^ 3", BinOp::Xor),
        ("1 |3", BinOp::Or),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![
                Node::BinaryOp(
                    Box::new(Node::Number(1.0)),
                    Box::new(Node::Number(3.0)),
                    op.clone(),
                ),
            ])
        );
    }
}

#[test]
fn simple_expr_shift() {
    use node::BinOp;

    for (input, op) in [
        ("1 << 2", BinOp::Shl),
        ("1 >> 2", BinOp::Shr),
        ("1 >>> 2", BinOp::ZFShr),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![
                Node::BinaryOp(
                    Box::new(Node::Number(1.0)),
                    Box::new(Node::Number(2.0)),
                    op.clone(),
                ),
            ])
        );
    }
}

#[test]
fn simple_expr_unary() {
    for (input, op) in [
        ("delete a", UnaryOp::Delete),
        ("void a", UnaryOp::Void),
        ("typeof a", UnaryOp::Typeof),
        ("+a", UnaryOp::Plus),
        ("-a", UnaryOp::Minus),
        ("~a", UnaryOp::BitwiseNot),
        ("!a", UnaryOp::Not),
        ("++a", UnaryOp::PrInc),
        ("--a", UnaryOp::PrDec),
        ("a++", UnaryOp::PoInc),
        ("a--", UnaryOp::PoDec),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![
                Node::UnaryOp(Box::new(Node::Identifier("a".to_string())), op.clone()),
            ])
        );
    }
}

#[test]
fn simple_expr_assign() {
    let mut parser = Parser::new("v = 1".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::Assign(
                Box::new(Node::Identifier("v".to_string())),
                Box::new(Node::Number(1.0)),
            ),
        ])
    );
}

#[test]
fn call() {
    for (input, args) in [
        ("f()", vec![]),
        ("f(1, 2, 3)", vec![1, 2, 3]),
        ("f(1, 2,)", vec![1, 2]),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![
                Node::Call(
                    Box::new(Node::Identifier("f".to_string())),
                    args.iter().map(|n| Node::Number(*n as f64)).collect(),
                ),
            ])
        );
    }
}

#[test]
fn member() {
    for (input, node) in [
        (
            "a.b.c",
            Node::Member(
                Box::new(Node::Member(
                    Box::new(Node::Identifier("a".to_string())),
                    "b".to_string(),
                )),
                "c".to_string(),
            ),
        ),
        (
            "console.log",
            Node::Member(
                Box::new(Node::Identifier("console".to_string())),
                "log".to_string(),
            ),
        ),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![node.clone()])
        );
    }
}

#[test]
fn if_() {
    use node::BinOp;

    let mut parser = Parser::new(
        "if (x <= 2) 
            then_stmt 
        else 
            else_stmt"
            .to_string(),
    );
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![
            Node::If(
                Box::new(Node::BinaryOp(
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
                Box::new(Node::BinaryOp(
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
