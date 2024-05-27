use crate::lexer::Token;
use crate::parser::Parser;

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
    Expression(Expression),
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

trait Parsable {
    fn parse(parser: &mut Parser) -> Result<impl Parsable, String>;
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Temp,
    Identifier(Identifier),
}

impl Expression {
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> Result<Expression, String> {
        match parser.curr_token() {
            Token::Ident(_) => Identifier::parse(parser).map(Expression::Identifier),
            _ => Err(format!("No parser available for token {}", parser.curr_token())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier(u32);

impl Identifier {
    pub fn new(value: u32) -> Identifier {
        Self(value)
    }
}

impl Parsable for Identifier {
    fn parse(parser: &mut Parser) -> Result<Identifier, String> {
        match parser.curr_token() {
            Token::Ident(id) => Ok(Identifier::new(*id)),
            _ => Err(format!("Expected identifier, got {}", parser.curr_token())),
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
            name: Identifier::new(u32::MAX),
            value: Expression::Temp,
        }
    }
}

impl LetStatement {
    pub fn new(name: u32, value: Expression) -> LetStatement {
        Self {
            name: Identifier::new(name),
            value,
        }
    }

    pub fn name(&self) -> u32 {
        self.name.0
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

pub enum Precedence {
    Lowest = 0,
    Equality = 1,
    Comparison = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}
