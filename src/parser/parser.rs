use crate::lexer::{ Lexer, Token };

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> &Parser {

    }

    fn next(&mut self) {

    }
}
