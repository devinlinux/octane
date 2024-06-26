use crate::lexer::{ Lexer, Token };
use crate::parser::ast::{
    Program,
    Statement,
    ParseStatement,
    LetStatement,
    ReturnStatement,
    Expression,
    Precedence,
};

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

//  API and related methods
impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Self {
            lexer,
            curr_token: Token::default(),
            peek_token: Token::default(),

            errors: Vec::new(),
        };
        parser.next();
        parser.next();

        parser
    }

    #[inline]
    pub(super) fn next(&mut self) {
        self.curr_token = std::mem::replace(&mut self.peek_token, self.lexer.next());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        while self.curr_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                program.add_statement(statement);
            }
            self.next();
        }

        program.set_lookup_table(self.lexer.take_lookup_table());
        program
    }

    pub(super) fn push_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    pub(super) fn lookup_literal(&self, key: usize) -> Option<&String> {
        self.lexer.lookup_literal(key)
    }

    pub fn curr_token(&self) -> &Token {
        &self.curr_token
    }

    pub fn peek_token(&self) -> Token {
        self.peek_token
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }
}

//  Statement parsing
impl Parser {
    pub(super) fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => LetStatement::parse(self).map(Statement::Let),
            Token::Return => ReturnStatement::parse(self).map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expression),
        }
    }
}

//  Expression parsing
impl Parser {
    fn parse_expression_statement(&mut self) -> Option<Expression> {
        let expression = Expression::parse(self, Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next();
        }

        match expression {
            Ok(expression) => Some(expression),
            Err(err) => {
                self.push_error(err);
                None
            },
        }
    }
}

//  helper methods
impl Parser {
    pub(super) fn curr_token_is(&self, token: &Token) -> bool {
        match self.curr_token {
            Token::Ident(_) => matches!(token, Token::Ident(_)),
            Token::Int(_) => matches!(token, Token::Int(_)),
            Token::Float(_) => matches!(token, Token::Float(_)),
            _ => &self.curr_token == token,
        }
    }

    pub(super) fn peek_token_is(&self, token: &Token) -> bool {
        match self.peek_token {
            Token::Ident(_) => matches!(token, Token::Ident(_)),
            Token::Int(_) => matches!(token, Token::Int(_)),
            Token::Float(_) => matches!(token, Token::Float(_)),
            _ => &self.peek_token == token,
        }
    }

    pub(super) fn assert_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next();
            return true
        }
        self.peek_error(token);
        false
    }

    #[inline]
    fn peek_error(&mut self, token: &Token) {
        self.push_error(format!("Expected next token to be {}, got {}", token, self.peek_token));
    }

    pub(super) fn curr_precedence(&self) -> Precedence {
        Precedence::from(&self.curr_token)
    }

    pub(super) fn peek_precedence(&self) -> Precedence {
        Precedence::from(&self.peek_token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{
        Identifier,
        IntegerLiteral,
        FloatLiteral,
        BooleanLiteral,
        FunctionLiteral,
        PrefixOperator,
        BlockStatement,
        CallExpression,
        ConditionalExpression,
        InfixOperator,
    };

    #[test]
    fn test_parse_let_statement() {
        let input = r#"
            let x = 5;
            let y = 7;
            let z = 42;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Let(LetStatement::new(0, Expression::IntegerLiteral(IntegerLiteral::new(5)))),
            Statement::Let(LetStatement::new(2, Expression::IntegerLiteral(IntegerLiteral::new(7)))),
            Statement::Let(LetStatement::new(4, Expression::IntegerLiteral(IntegerLiteral::new(42)))),
        ];

        statement_assert_loop(program, expected_statements)
    }

    #[test]
    fn test_parse_return_statement() {
        let input = r#"
            return 5;
            return 7;
            return 42;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(IntegerLiteral::new(5)))),
            Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(IntegerLiteral::new(7)))),
            Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(IntegerLiteral::new(42)))),
        ];

        statement_assert_loop(program, expected_statements);
    }

    #[test]
    fn test_parse_identifier_expression() {
        let input = r#"
            x;
            y;
            z;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::Identifier(Identifier::new(0))),
            Statement::Expression(Expression::Identifier(Identifier::new(1))),
            Statement::Expression(Expression::Identifier(Identifier::new(2))),
        ];

        statement_assert_loop(program, expected_statements);
    }

    #[test]
    fn test_parse_numbers() {
        let input = r#"
            7;
            3.14;
            42;
            1_000_000;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::IntegerLiteral(IntegerLiteral::new(7))),
            Statement::Expression(Expression::FloatLiteral(FloatLiteral::new(3.14))),
            Statement::Expression(Expression::IntegerLiteral(IntegerLiteral::new(42))),
            Statement::Expression(Expression::IntegerLiteral(IntegerLiteral::new(1000000))),
        ];

        statement_assert_loop(program, expected_statements)
    }

    #[test]
    fn test_parse_boolean() {
        let input = r#"
            true;
            false;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::BooleanLiteral(BooleanLiteral::new(true))),
            Statement::Expression(Expression::BooleanLiteral(BooleanLiteral::new(false))),
        ];

        statement_assert_loop(program, expected_statements)
    }

    #[test]
    fn test_parse_prefix_operator() {
        let input = r#"
            -5;
            !bad;
            -42;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::PrefixOperator(PrefixOperator::new(Token::Minus,
                        Expression::IntegerLiteral(IntegerLiteral::new(5))
            ))),
            Statement::Expression(Expression::PrefixOperator(PrefixOperator::new(Token::Bang,
                        Expression::Identifier(Identifier::new(1))
            ))),
            Statement::Expression(Expression::PrefixOperator(PrefixOperator::new(Token::Minus,
                        Expression::IntegerLiteral(IntegerLiteral::new(42))
            ))),
        ];

        statement_assert_loop(program, expected_statements)
    }

    #[test]
    fn test_parse_infix_operator() {
        let input = r#"
            5 - 5;
            7 * 7;
            1 < 2;
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::InfixOperator(InfixOperator::new(Token::Minus,
                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                        Expression::IntegerLiteral(IntegerLiteral::new(5))
            ))),
            Statement::Expression(Expression::InfixOperator(InfixOperator::new(Token::Asterisk,
                        Expression::IntegerLiteral(IntegerLiteral::new(7)),
                        Expression::IntegerLiteral(IntegerLiteral::new(7))
            ))),
            Statement::Expression(Expression::InfixOperator(InfixOperator::new(Token::LT,
                        Expression::IntegerLiteral(IntegerLiteral::new(1)),
                        Expression::IntegerLiteral(IntegerLiteral::new(2)),
            ))),
        ];

        statement_assert_loop(program, expected_statements);
    }

    #[test]
    fn test_parse_conditional() {
        let input = r#"
            if (4 < 5) {
                !true;
            } else {
                !false;
            }
            if x > 4._010 {
                !false;
            }
            "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);

        let expected_statements = vec![
            Statement::Expression(Expression::ConditionalExpression(ConditionalExpression::new(
                        Expression::InfixOperator(InfixOperator::new(Token::LT,
                                    Expression::IntegerLiteral(IntegerLiteral::new(4)),
                                    Expression::IntegerLiteral(IntegerLiteral::new(5))
                        )),
                        BlockStatement::new(vec![
                            Statement::Expression(Expression::PrefixOperator(PrefixOperator::new(Token::Bang,
                                            Expression::BooleanLiteral(BooleanLiteral::new(true))))),
                        ]),
                        Some(BlockStatement::new(vec![
                            Statement::Expression(Expression::PrefixOperator(PrefixOperator::new(Token::Bang,
                                            Expression::BooleanLiteral(BooleanLiteral::new(false))))),
                        ]))
            ))),
            Statement::Expression(Expression::ConditionalExpression(ConditionalExpression::new(
                        Expression::InfixOperator(InfixOperator::new(Token::GT,
                                Expression::Identifier(Identifier::new(2)),
                                Expression::FloatLiteral(FloatLiteral::new(4.010))
                        )),
                        BlockStatement::new(vec![
                            Statement::Expression(Expression::PrefixOperator(PrefixOperator::new(Token::Bang,
                                            Expression::BooleanLiteral(BooleanLiteral::new(false))))),
                        ]),
                        None
            ))),
        ];

        let program = parser.parse_program();
        statement_assert_loop(program, expected_statements)
    }

    #[test]
    fn test_parse_function() {
        let input = r#"
            fn(a, b, c) {
                return 777;
            }
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::FunctionLiteral(FunctionLiteral::new(
                vec![
                    Identifier::new(0),
                    Identifier::new(1),
                    Identifier::new(2),
                ],
                BlockStatement::new(vec![
                    Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(IntegerLiteral::new(777)))),
                ])
            ))),
        ];

        statement_assert_loop(program, expected_statements)
    }

    #[test]
    fn test_parse_function_call() {
        let input = r#"
            call(a, b, c);
            call(2 + 2);
            "#;
        let program = parse_input(input);

        let expected_statements = vec![
            Statement::Expression(Expression::CallExpression(CallExpression::new(
                Expression::Identifier(Identifier::new(0)),
                vec![
                    Expression::Identifier(Identifier::new(1)),
                    Expression::Identifier(Identifier::new(2)),
                    Expression::Identifier(Identifier::new(3)),
                ],
            ))),
            Statement::Expression(Expression::CallExpression(CallExpression::new(
                Expression::Identifier(Identifier::new(4)),
                vec![
                    Expression::InfixOperator(InfixOperator::new(Token::Plus,
                            Expression::IntegerLiteral(IntegerLiteral::new(2)),
                            Expression::IntegerLiteral(IntegerLiteral::new(2))
                    )),
                ],
            ))),
        ];

        statement_assert_loop(program, expected_statements);
    }

    fn parse_input(input: &str) -> Program {
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    fn statement_assert_loop(program: Program, expected_statements: Vec<Statement>) {
        let statements = program.statements();

        assert_eq!(expected_statements.len(), statements.len());
        for (expected, actual) in expected_statements.iter().zip(statements) {
            assert_eq!(expected, actual);
        }
    }
}
