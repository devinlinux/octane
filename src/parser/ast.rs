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

impl Program {
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Statement {
    pub fn try_into_let(&self) -> Result<&LetStatement, &Self> {
        if let Self::Let(stmt) = self {
            Ok(stmt)
        } else {
            Err(self)
        }
    }

    pub fn try_into_return(&self) -> Result<&ReturnStatement, &Self> {
        if let Self::Return(stmt) = self {
            Ok(stmt)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Temp,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

    pub fn name(&self) -> u32 {
        self.name.value
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    value: Expression,
}

impl ReturnStatement {
    pub fn new(value: Expression) -> ReturnStatement {
        Self {
            value
        }
    }
}
