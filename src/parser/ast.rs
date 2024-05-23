use crate::lexer::Token;

pub struct Program {
    statements: Vec<Statement>,
}

impl Default for Program {
    fn default() -> Program {
        Self {
            statements: Vec::new(),
        }
    }
}

pub enum Statement {
    Let(LetStatement),
}

pub enum Expression {
    Temp,
}

pub struct Identifier {
    token: Token,
    value: u32,
}

impl Identifier {
    pub fn new(token: Token, value: u32) -> Identifier {
        Self {
            token,
            value,
        }
    }
}

pub struct LetStatement {
    name: Identifier,
    value: Expression,
}

impl Default for LetStatement {
    fn default() -> LetStatement {
        Self {
            name: Identifier::new(Token::Let, u32::MAX),
            value: Expression::Temp,
        }
    }
}

impl LetStatement {
    pub fn new(name: u32, value: Expression) -> LetStatement {
        Self {
            name: Identifier::new(Token::Let, name),
            value,
        }
    }
}
