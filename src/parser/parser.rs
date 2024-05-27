use crate::lexer::{ Lexer, Token };
use crate::parser::ast::{
    Program,
    Statement,
    LetStatement,
    ReturnStatement,
    Expression,
    Identifier,
    IntegerLiteral,
    FloatLiteral,
    Precedence
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
    fn next(&mut self) {
        self.curr_token = std::mem::replace(&mut self.peek_token, self.lexer.next());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        while self.curr_token != Token::Eof {
            match self.parse_statement() {
                Some(statement) => program.add_statement(statement),
                None => continue,
            }
            self.next();
        }

        program
    }

    fn push_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    pub(super) fn lookup_literal(&self, key: usize) -> Option<&String> {
        self.lexer.lookup_literal(key)
    }

    pub fn curr_token(&self) -> &Token {
        &self.curr_token
    }
}

//  Statement parsing
impl Parser {
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement().map(Statement::Let),
            Token::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expression),
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if !self.assert_peek(&Token::Ident(0)) {
            return None
        }

        let name = match self.curr_token {
            Token::Ident(name) => name,
            _ => unreachable!("Identity as Ident should have already been confirmed"),
        };

        if !self.assert_peek(&Token::Assign) {
            return None
        }

        while !self.curr_token_is(&Token::Semicolon) {
            self.next();
        }

        Some(LetStatement::new(name, Expression::Temp))
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        self.next();

        while !self.curr_token_is(&Token::Semicolon) {
            self.next();
        }

        Some(ReturnStatement::new(Expression::Temp))
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
    fn curr_token_is(&self, token: &Token) -> bool {
        match self.curr_token {
            Token::Ident(_) => matches!(token, Token::Ident(_)),
            Token::Int(_) => matches!(token, Token::Int(_)),
            Token::Float(_) => matches!(token, Token::Float(_)),
            _ => &self.curr_token == token,
        }
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match self.peek_token {
            Token::Ident(_) => matches!(token, Token::Ident(_)),
            Token::Int(_) => matches!(token, Token::Int(_)),
            Token::Float(_) => matches!(token, Token::Float(_)),
            _ => &self.peek_token == token,
        }
    }

    fn assert_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next();
            return true
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let input = r#"
            let x = 5;
            let y = 7;
            let z = 42;
            "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);

        let expected_statements = vec![
            Statement::Let(LetStatement::new(0, Expression::Temp)),
            Statement::Let(LetStatement::new(2, Expression::Temp)),
            Statement::Let(LetStatement::new(4, Expression::Temp)),
        ];

        let expected_names = vec![
            "x",
            "y",
            "z",
        ];

        statement_assert_loop(parser.parse_program(), expected_statements);
    }

    #[test]
    fn test_parse_return_statement() {
        let input = r#"
            return 5;
            return 7;
            return 42;
            "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);

        let expected_statements = vec![
            Statement::Return(ReturnStatement::new(Expression::Temp)),
            Statement::Return(ReturnStatement::new(Expression::Temp)),
            Statement::Return(ReturnStatement::new(Expression::Temp)),
        ];

        statement_assert_loop(parser.parse_program(), expected_statements);
    }

    #[test]
    fn test_parse_identifier_expression() {
        let input = r#"
            x;
            y;
            z;
            "#;
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);

            let expected_statements = vec![
                Statement::Expression(Expression::Identifier(Identifier::new(0))),
                Statement::Expression(Expression::Identifier(Identifier::new(1))),
                Statement::Expression(Expression::Identifier(Identifier::new(2))),
            ];

            statement_assert_loop(parser.parse_program(), expected_statements);
    }

    #[test]
    fn test_parse_numbers() {
        let input = r#"
            7;
            3.14;
            42;
            1_000_000;
            "#;
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);

            let expected_statements = vec![
                Statement::Expression(Expression::IntegerLiteral(IntegerLiteral::new(7))),
                Statement::Expression(Expression::FloatLiteral(FloatLiteral::new(3.14))),
                Statement::Expression(Expression::IntegerLiteral(IntegerLiteral::new(42))),
                Statement::Expression(Expression::IntegerLiteral(IntegerLiteral::new(1000000))),
            ];

            statement_assert_loop(parser.parse_program(), expected_statements)
    }

    fn statement_assert_loop(program: Program, expected_statements: Vec<Statement>) {
            let statements = program.statements();

            assert_eq!(expected_statements.len(), statements.len());
            for (expected, actual) in expected_statements.iter().zip(statements) {
                assert_eq!(expected, actual);
            }
    }
}
