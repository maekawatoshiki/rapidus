use lexer;
use node::{BinOp, FormalParameter, FormalParameters, Node, PropertyDefinition, UnaryOp};
use std::collections::HashSet;
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

            if self.lexer.skip(Kind::Symbol(Symbol::ClosingBrace)) {
                break;
            }

            if let Ok(item) = self.read_statement_list_item() {
                items.push(item)
            }

            self.lexer.skip(Kind::Symbol(Symbol::Semicolon));
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
            Kind::Keyword(Keyword::Var) => self.read_variable_statement(),
            Kind::Keyword(Keyword::While) => self.read_while_statement(),
            Kind::Keyword(Keyword::Return) => self.read_return_statement(),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_block_statement(),
            _ => {
                self.lexer.unget(&tok);
                self.read_expression_statement()
            }
        }
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-BlockStatement
    fn read_block_statement(&mut self) -> Result<Node, ()> {
        self.read_statement_list()
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-VariableStatement
    fn read_variable_statement(&mut self) -> Result<Node, ()> {
        self.read_variable_declaration_list()
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclarationList
    fn read_variable_declaration_list(&mut self) -> Result<Node, ()> {
        let mut list = vec![];

        loop {
            list.push(self.read_variable_declaration()?);
            if !self.lexer.skip(Kind::Symbol(Symbol::Comma)) {
                break;
            }
        }

        Ok(Node::StatementList(list))
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclaration
    fn read_variable_declaration(&mut self) -> Result<Node, ()> {
        let name = match self.lexer.next()?.kind {
            Kind::Identifier(name) => name,
            _ => unimplemented!(),
        };

        if self.lexer.skip(Kind::Symbol(Symbol::Assign)) {
            Ok(Node::VarDecl(
                name,
                Some(Box::new(self.read_initializer()?)),
            ))
        } else {
            Ok(Node::VarDecl(name, None))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-Initializer
    fn read_initializer(&mut self) -> Result<Node, ()> {
        self.read_assignment_expression()
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
            Box::new(Node::Nope),
        ))
    }
}

impl Parser {
    fn read_while_statement(&mut self) -> Result<Node, ()> {
        assert_eq!(self.lexer.next()?.kind, Kind::Symbol(Symbol::OpeningParen));
        let cond = self.read_expression()?;
        assert_eq!(self.lexer.next()?.kind, Kind::Symbol(Symbol::ClosingParen));

        let body = self.read_statement()?;

        Ok(Node::While(Box::new(cond), Box::new(body)))
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

    /// https://tc39.github.io/ecma262/#prod-Expression
    expression!(read_expression, read_assignment_expression, [Symbol::Comma]);

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
        let lhs = self.read_new_expression()?;

        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-NewExpression
    fn read_new_expression(&mut self) -> Result<Node, ()> {
        if self.lexer.skip(Kind::Keyword(Keyword::New)) {
            Ok(Node::New(Box::new(self.read_new_expression()?)))
        } else {
            self.read_call_expression()
        }
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
            Kind::Keyword(Keyword::This) => Ok(Node::This),
            Kind::Keyword(Keyword::Function) => self.read_function_expression(),
            Kind::Symbol(Symbol::Semicolon) => Ok(Node::Nope),
            Kind::Symbol(Symbol::OpeningParen) => {
                let x = self.read_expression();
                self.lexer.skip(Kind::Symbol(Symbol::ClosingParen));
                x
            }
            Kind::Symbol(Symbol::OpeningBoxBracket) => self.read_array_literal(),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_object_literal(),
            Kind::Identifier(ref i) if i == "true" => Ok(Node::Boolean(true)),
            Kind::Identifier(ref i) if i == "false" => Ok(Node::Boolean(false)),
            Kind::Identifier(ident) => Ok(Node::Identifier(ident)),
            Kind::String(s) => Ok(Node::String(s)),
            Kind::Number(num) => Ok(Node::Number(num)),
            Kind::LineTerminator => self.read_primary_expression(),
            e => unimplemented!("{:?}", e),
        }
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_expression(&mut self) -> Result<Node, ()> {
        let name = if let Kind::Identifier(name) = self.lexer.peek()?.kind {
            self.lexer.next()?;
            Some(name)
        } else {
            None
        };

        assert!(self.lexer.skip(Kind::Symbol(Symbol::OpeningParen)));
        let params = self.read_formal_parameters()?;

        assert!(self.lexer.skip(Kind::Symbol(Symbol::OpeningBrace)));
        let body = self.read_statement_list()?;

        Ok(Node::FunctionExpr(name, params, Box::new(body)))
    }

    /// https://tc39.github.io/ecma262/#prod-ArrayLiteral
    fn read_array_literal(&mut self) -> Result<Node, ()> {
        let mut elements = vec![];

        loop {
            // TODO: Support all features.
            while self.lexer.skip(Kind::Symbol(Symbol::Comma)) {
                elements.push(Node::Nope);
            }

            if self.lexer.skip(Kind::Symbol(Symbol::ClosingBoxBracket)) {
                break;
            }

            if let Ok(elem) = self.read_assignment_expression() {
                elements.push(elem);
            }

            self.lexer.skip(Kind::Symbol(Symbol::Comma));
        }

        Ok(Node::Array(elements))
    }

    /// https://tc39.github.io/ecma262/#prod-ObjectLiteral
    fn read_object_literal(&mut self) -> Result<Node, ()> {
        let mut elements = vec![];

        loop {
            if self.lexer.skip(Kind::Symbol(Symbol::ClosingBrace)) {
                break;
            }
            if let Ok(elem) = self.read_property_definition() {
                elements.push(elem);
            }
            self.lexer.skip(Kind::Symbol(Symbol::Comma));
        }

        Ok(Node::Object(elements))
    }

    /// https://tc39.github.io/ecma262/#prod-PropertyDefinition
    fn read_property_definition(&mut self) -> Result<PropertyDefinition, ()> {
        fn to_string(kind: Kind) -> String {
            match kind {
                Kind::Identifier(name) => name,
                Kind::Number(n) => format!("{}", n),
                Kind::String(s) => s,
                _ => unimplemented!(),
            }
        }

        let tok = self.lexer.next()?;

        if self.lexer.skip(Kind::Symbol(Symbol::Colon)) {
            let val = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::Property(to_string(tok.kind), val));
        }

        if let Kind::Identifier(name) = tok.kind {
            return Ok(PropertyDefinition::IdentifierReference(name));
        }

        // TODO: Support all features.
        Err(())
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-ReturnStatement
    fn read_return_statement(&mut self) -> Result<Node, ()> {
        if self.lexer.skip(Kind::Symbol(Symbol::Semicolon)) {
            return Ok(Node::Return(None));
        }

        let expr = self.read_expression()?;
        self.lexer.skip(Kind::Symbol(Symbol::Semicolon));

        Ok(Node::Return(Some(Box::new(expr))))
    }
}

impl Parser {
    fn is_declaration(&mut self) -> bool {
        self.is_hoistable_declaration()
    }

    fn read_declaration(&mut self) -> Result<Node, ()> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Keyword(Keyword::Function) => self.read_function_declaration(),
            _ => Ok(Node::Nope),
        }
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_declaration(&mut self) -> Result<Node, ()> {
        let name = if let Kind::Identifier(name) = self.lexer.next()?.kind {
            name
        } else {
            panic!("expected function name")
        };

        assert!(self.lexer.skip(Kind::Symbol(Symbol::OpeningParen)));
        let params = self.read_formal_parameters()?;

        assert!(self.lexer.skip(Kind::Symbol(Symbol::OpeningBrace)));
        let body = self.read_statement_list()?;

        Ok(Node::FunctionDecl(
            name,
            false,
            HashSet::new(),
            params,
            Box::new(body),
        ))
    }

    fn read_formal_parameters(&mut self) -> Result<FormalParameters, ()> {
        if self.lexer.skip(Kind::Symbol(Symbol::ClosingParen)) {
            return Ok(vec![]);
        }

        let mut params = vec![];

        loop {
            params.push(self.read_formal_parameter()?);

            if self.lexer.skip(Kind::Symbol(Symbol::ClosingParen)) {
                break;
            }

            assert!(self.lexer.skip(Kind::Symbol(Symbol::Comma)))
        }

        Ok(params)
    }

    // TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
    pub fn read_formal_parameter(&mut self) -> Result<FormalParameter, ()> {
        let name = if let Kind::Identifier(name) = self.lexer.next()?.kind {
            name
        } else {
            panic!()
        };
        // TODO: Implement initializer.
        Ok(FormalParameter::new(name, None))
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
fn string() {
    let mut parser = Parser::new("\"aaa\"".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::String("aaa".to_string())])
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
fn array1() {
    let mut parser = Parser::new("[1, 2]".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Array(vec![
            Node::Number(1.0),
            Node::Number(2.0),
        ])])
    );
}

#[test]
fn array2() {
    let mut parser = Parser::new("[]".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Array(vec![])])
    );
}

#[test]
fn array3() {
    let mut parser = Parser::new("[,,]".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Array(vec![Node::Nope, Node::Nope])])
    );
}

#[test]
fn object() {
    let mut parser = Parser::new("a = {x: 123, 1.2: 456}".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Assign(
            Box::new(Node::Identifier("a".to_string())),
            Box::new(Node::Object(vec![
                PropertyDefinition::Property("x".to_string(), Node::Number(123.0)),
                PropertyDefinition::Property("1.2".to_string(), Node::Number(456.0)),
            ])),
        )])
    );
}

#[test]
fn simple_expr_5arith() {
    use node::BinOp;

    let mut parser = Parser::new("31 + 26 / 3 - 1 * 20 % 3".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::BinaryOp(
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
        )])
    );
}

#[test]
fn simple_expr_eq() {
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
            Node::StatementList(vec![Node::BinaryOp(
                Box::new(Node::BinaryOp(
                    Box::new(Node::Number(1.0)),
                    Box::new(Node::Number(2.0)),
                    BinOp::Add,
                )),
                Box::new(Node::Number(3.0)),
                op.clone(),
            )])
        );
    }
}

#[test]
fn simple_expr_rel() {
    for (input, op) in [
        ("1 + 2 < 3", BinOp::Lt),
        ("1 + 2 > 3", BinOp::Gt),
        ("1 + 2 <= 3", BinOp::Le),
        ("1 + 2 >= 3", BinOp::Ge),
    ].iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![Node::BinaryOp(
                Box::new(Node::BinaryOp(
                    Box::new(Node::Number(1.0)),
                    Box::new(Node::Number(2.0)),
                    BinOp::Add,
                )),
                Box::new(Node::Number(3.0)),
                op.clone(),
            )])
        );
    }
}

#[test]
fn simple_expr_cond() {
    use node::BinOp;

    let mut parser = Parser::new("n == 1 ? 2 : max".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::TernaryOp(
            Box::new(Node::BinaryOp(
                Box::new(Node::Identifier("n".to_string())),
                Box::new(Node::Number(1.0)),
                BinOp::Eq,
            )),
            Box::new(Node::Number(2.0)),
            Box::new(Node::Identifier("max".to_string())),
        )])
    );
}

#[test]
fn simple_expr_logical_or() {
    use node::BinOp;

    for (input, op) in [("1 || 0", BinOp::LOr), ("1 && 0", BinOp::LAnd)].iter() {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![Node::BinaryOp(
                Box::new(Node::Number(1.0)),
                Box::new(Node::Number(0.0)),
                op.clone(),
            )])
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
            Node::StatementList(vec![Node::BinaryOp(
                Box::new(Node::Number(1.0)),
                Box::new(Node::Number(3.0)),
                op.clone(),
            )])
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
            Node::StatementList(vec![Node::BinaryOp(
                Box::new(Node::Number(1.0)),
                Box::new(Node::Number(2.0)),
                op.clone(),
            )])
        );
    }
}

#[test]
fn simple_exp() {
    for (input, op) in [("2**5", BinOp::Exp)].iter() {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.next().unwrap(),
            Node::StatementList(vec![Node::BinaryOp(
                Box::new(Node::Number(2.0)),
                Box::new(Node::Number(5.0)),
                op.clone(),
            )])
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
            Node::StatementList(vec![Node::UnaryOp(
                Box::new(Node::Identifier("a".to_string())),
                op.clone(),
            )])
        );
    }
}

#[test]
fn simple_expr_assign() {
    let mut parser = Parser::new("v = 1".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::Assign(
            Box::new(Node::Identifier("v".to_string())),
            Box::new(Node::Number(1.0)),
        )])
    );
}

#[test]
fn simple_expr_new() {
    let mut parser = Parser::new("new f(1)".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::New(Box::new(Node::Call(
            Box::new(Node::Identifier("f".to_string())),
            vec![Node::Number(1.0)],
        )))])
    );
}

#[test]
fn simple_expr_parentheses() {
    let mut parser = Parser::new("2 * (1 + 3)".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::BinaryOp(
            Box::new(Node::Number(2.0)),
            Box::new(Node::BinaryOp(
                Box::new(Node::Number(1.0)),
                Box::new(Node::Number(3.0)),
                BinOp::Add,
            )),
            BinOp::Mul,
        )])
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
            Node::StatementList(vec![Node::Call(
                Box::new(Node::Identifier("f".to_string())),
                args.iter().map(|n| Node::Number(*n as f64)).collect(),
            )])
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
fn var_decl() {
    let mut parser = Parser::new("var a, b = 21".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::StatementList(vec![
            Node::VarDecl("a".to_string(), None),
            Node::VarDecl("b".to_string(), Some(Box::new(Node::Number(21.0)))),
        ])])
    );
}

#[test]
fn block() {
    let mut parser = Parser::new("{ a=1 }".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::StatementList(vec![Node::Assign(
            Box::new(Node::Identifier("a".to_string())),
            Box::new(Node::Number(1.0)),
        )])])
    );
}

#[test]
fn return_() {
    for (input, node) in [
        ("return 1", Node::Return(Some(Box::new(Node::Number(1.0))))),
        ("return;", Node::Return(None)),
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
        Node::StatementList(vec![Node::If(
            Box::new(Node::BinaryOp(
                Box::new(Node::Identifier("x".to_string())),
                Box::new(Node::Number(2.0)),
                BinOp::Le,
            )),
            Box::new(Node::Identifier("then_stmt".to_string())),
            Box::new(Node::Identifier("else_stmt".to_string())),
        )])
    );

    parser = Parser::new("if (x <= 2) then_stmt ".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::If(
            Box::new(Node::BinaryOp(
                Box::new(Node::Identifier("x".to_string())),
                Box::new(Node::Number(2.0)),
                BinOp::Le,
            )),
            Box::new(Node::Identifier("then_stmt".to_string())),
            Box::new(Node::Nope),
        )])
    );
}

#[test]
fn while_() {
    let mut parser = Parser::new("while (true) { }".to_string());
    assert_eq!(
        parser.next().unwrap(),
        Node::StatementList(vec![Node::While(
            Box::new(Node::Boolean(true)),
            Box::new(Node::StatementList(vec![])),
        )])
    );
}

#[test]
fn function_decl() {
    for (input, node) in [
        (
            "function f() { }",
            Node::FunctionDecl(
                "f".to_string(),
                false,
                HashSet::new(),
                vec![],
                Box::new(Node::StatementList(vec![])),
            ),
        ),
        (
            "function f(x, y) { return x + y }",
            Node::FunctionDecl(
                "f".to_string(),
                false,
                HashSet::new(),
                vec![
                    FormalParameter::new("x".to_string(), None),
                    FormalParameter::new("y".to_string(), None),
                ],
                Box::new(Node::StatementList(vec![Node::Return(Some(Box::new(
                    Node::BinaryOp(
                        Box::new(Node::Identifier("x".to_string())),
                        Box::new(Node::Identifier("y".to_string())),
                        BinOp::Add,
                    ),
                )))])),
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
