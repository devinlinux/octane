use crate::lexer::Token;

pub struct Program<'a> {
    statements: Vec<Statement<'a>>,
}

impl<'a> Default for Program<'a> {
    fn default() -> Program<'a> {
        Self {
            statements: Vec::new(),
        }
    }
}

pub enum Statement<'a> {
    Let(LetStatement<'a>),
}

pub enum Expression {
    Temp,
}

pub struct Identifier<'a> {
    token: Token,
    value: &'a str,
}

impl<'a> Identifier<'a> {
    pub fn new(token: Token, value: &'a str) -> Identifier<'a> {
        Self {
            token,
            value,
        }
    }
}

pub struct LetStatement<'a> {
    name: Identifier<'a>,
    value: Expression,
}

impl<'a> Default for LetStatement<'a> {
    fn default() -> LetStatement<'a> {
        Self {
            name: Identifier::new(Token::Let, ""),
            value: Expression::Temp,
        }
    }
}

impl<'a> LetStatement<'a> {
    pub fn new(name: &'a str, value: Expression) -> LetStatement<'a> {
        Self {
            name: Identifier::new(Token::Let, name),
            value,
        }
    }
}
