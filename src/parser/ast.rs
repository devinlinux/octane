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

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    name: Identifier,
    value: Expression,
}

impl Default for LetStatement {
    fn default() -> LetStatement {
        Self {
            name: Identifier::new(0),
            value: Expression::Identifier(Identifier::new(0)),
        }
    }
}

impl LetStatement {
    pub fn new(name: usize, value: Expression) -> LetStatement {
        Self {
            name: Identifier::new(name),
            value,
        }
    }

    pub fn name(&self) -> usize {
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

trait Parsable {
    fn parse(parser: &mut Parser) -> Result<impl Parsable, String>;
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

#[derive(Debug, PartialEq)]
pub enum Expression {
    Temp,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
}

impl Expression {
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> Result<Expression, String> {
        match parser.curr_token() {
            Token::Ident(_) => Identifier::parse(parser).map(Expression::Identifier),
            Token::Int(_) => IntegerLiteral::parse(parser).map(Expression::IntegerLiteral),
            Token::Float(_) => FloatLiteral::parse(parser).map(Expression::FloatLiteral),
            _ => Err(format!("No parser available for token {}", parser.curr_token())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier(usize);

impl Identifier {
    pub fn new(value: usize) -> Identifier {
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
pub struct IntegerLiteral(i64);

impl IntegerLiteral {
    pub fn new(value: i64) -> IntegerLiteral {
        Self(value)
    }
}

impl Parsable for IntegerLiteral {
    fn parse(parser: &mut Parser) -> Result<IntegerLiteral, String> {
        match parser.curr_token() {
            Token::Int(key) => {
                let number = parser.lookup_literal(*key).expect("Integer was not registered by lexer");
                match number.parse::<i64>() {
                    Ok(int) => Ok(Self(int)),
                    Err(err) => Err(format!("Could not parse {} as integer literal: {}", number, err)),
                }
            },
            _ => unreachable!("Should only attempt to parse IntegerLiteral on Integer token"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FloatLiteral(f64);

impl FloatLiteral {
    pub fn new(value: f64) -> FloatLiteral {
        Self(value)
    }
}

impl Parsable for FloatLiteral {
    fn parse(parser: &mut Parser) -> Result<FloatLiteral, String> {
        match parser.curr_token() {
            Token::Float(key) => {
                let number = parser.lookup_literal(*key).expect("Float was not registered by lexer");
                match number.parse::<f64>() {
                    Ok(float) => Ok(Self(float)),
                    Err(err) => Err(format!("Could not parse {} as float literal: {}", number, err)),
                }
            },
            _ => unreachable!("Should only attempt to parse FloatLiteral on Float token"),
        }
    }
}
