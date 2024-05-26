use crate::lexer::{ Lexer, Token };
use crate::parser::ast::{ Program, Statement, LetStatement, ReturnStatement, Expression };

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
}

//  API and related methods
impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Self {
            lexer,
            curr_token: Token::default(),
            peek_token: Token::default(),
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
}

//  Statement parsing
impl Parser {
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement().map(Statement::Let),
            Token::Return => self.parse_return_statement().map(Statement::Return),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if !self.assert_peek(&Token::Ident(u32::MAX)) {
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
            Statement::Let(LetStatement::new(1, Expression::Temp)),
            Statement::Let(LetStatement::new(2, Expression::Temp)),
        ];

        let expected_names = vec![
            "x",
            "y",
            "z",
        ];

        let program = parser.parse_program();
        let statements = program.statements();

        assert_eq!(expected_statements.len(), statements.len());
        for ((expected_statement, expected_name), actual) in expected_statements.iter().zip(&expected_names).zip(statements) {
            assert_eq!(expected_statement, actual);
            let name = parser.lexer.lookup_ident(actual.try_into_let().expect("Could not convert to let statement").name()).expect("Did not find identifier from index");
            assert_eq!(name, expected_name);
        }
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

        let program = parser.parse_program();
        let statements = program.statements();

        assert_eq!(expected_statements.len(), statements.len());
        for (expected, actual) in expected_statements.iter().zip(statements) {
            assert_eq!(expected, actual);
        }
    }
}
