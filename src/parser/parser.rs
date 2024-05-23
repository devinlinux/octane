use crate::lexer::{ Lexer, Token };
use crate::parser::ast::{ Program, Statement, LetStatement, Identifier, Expression };

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
            let statement = self.parse_statement();
            
        }

        program
    }
}

//  Statement parsing
impl Parser {
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {

        if !self.assert_peek(&Token::Ident(String::new())) {
            return None
        }

        let name = match self.curr_token {
            Token::Ident(ref name) => name,
            _ => unreachable!("Check for ident token has already been performed"),
        };

        if !self.assert_peek(&Token::Assign) {
            return None
        }

        while !self.curr_token_is(&Token::Semicolon) {
            self.next();
        }

        Some(LetStatement::new(&name, Expression::Temp))
    }
}

//  helper methods
impl Parser {
    #[inline]
    fn curr_token_is(&self, token: &Token) -> bool {
        match self.curr_token {
            Token::Ident(_) => matches!(token, Token::Ident(_)),
            Token::Int(_) => matches!(token, Token::Int(_)),
            Token::Float(_) => matches!(token, Token::Float(_)),
            _ => &self.curr_token == token,
        }
    }

    #[inline]
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
