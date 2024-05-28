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
    type Output: Parsable;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String>;

    fn parse_with_lhs(parser: &mut Parser, _lhs: Expression) -> Result<Self::Output, String> {
        Self::parse(parser)
    }
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equality = 1,
    Comparison = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Precedence {
        match value {
            Token::Eq | Token::NotEq => Self::Equality,
            Token::LT | Token::GT => Self::Comparison,
            Token::Plus | Token::Minus => Self::Sum,
            Token::Asterisk | Token::Slash => Self::Product,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Temp,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    BooleanLiteral(BooleanLiteral),
    PrefixOperator(PrefixOperator),
    InfixOperator(InfixOperator),
}

impl Expression {
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> Result<Expression, String> {
        let mut lhs = match parser.curr_token() {
            Token::Ident(_) => Identifier::parse(parser).map(Expression::Identifier),
            Token::Int(_) => IntegerLiteral::parse(parser).map(Expression::IntegerLiteral),
            Token::Float(_) => FloatLiteral::parse(parser).map(Expression::FloatLiteral),
            Token::True | Token::False => BooleanLiteral::parse(parser).map(Expression::BooleanLiteral),
            Token::Bang | Token::Minus => PrefixOperator::parse(parser).map(Expression::PrefixOperator),
            _ => Err(format!("No parser available for token {}", parser.curr_token())),
        }?;

        while !parser.peek_token_is(&Token::Semicolon) && precedence < parser.peek_precedence() {
            match parser.peek_token() {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::LT
                | Token::GT => {
                    parser.next();
                    lhs = InfixOperator::parse_with_lhs(parser, lhs).map(Expression::InfixOperator)?;
                },
                _ => return Ok(lhs),
            }
        }

        Ok(lhs)
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
    type Output = Identifier;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        match parser.curr_token() {
            Token::Ident(id) => Ok(Self(*id)),
            _ => Err(format!("Expected identifier, got {}", parser.curr_token())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral(i64);

#[cfg(test)]
impl IntegerLiteral {
    pub fn new(value: i64) -> IntegerLiteral {
        Self(value)
    }
}

impl Parsable for IntegerLiteral {
    type Output = IntegerLiteral;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
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

#[cfg(test)]
impl FloatLiteral {
    pub fn new(value: f64) -> FloatLiteral {
        Self(value)
    }
}

impl Parsable for FloatLiteral {
    type Output = FloatLiteral;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
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

#[derive(Debug, PartialEq)]
pub struct PrefixOperator {
    operator: Token,
    rhs: Box<Expression>,
}

impl PrefixOperator {
    pub fn new(operator: Token, rhs: Expression) -> PrefixOperator {
        Self {
            operator,
            rhs: Box::new(rhs),
        }
    }
}

impl Parsable for PrefixOperator {
    type Output = PrefixOperator;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        let operator = *parser.curr_token();

        parser.next();
        let rhs = Expression::parse(parser, Precedence::Prefix)?;

        Ok(PrefixOperator::new(operator, rhs))
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixOperator {
    operator: Token,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl InfixOperator {
    pub fn new(operator: Token, lhs: Expression, rhs: Expression) -> InfixOperator {
        Self {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl Parsable for InfixOperator {
    type Output = InfixOperator;

    fn parse(_parser: &mut Parser) -> Result<Self::Output, String> {
        unimplemented!()
    }

    fn parse_with_lhs(parser: &mut Parser, lhs: Expression) -> Result<Self::Output, String> {
        let operator = *parser.curr_token();  //  copy
        let precedence = parser.curr_precedence();

        parser.next();
        let rhs = Expression::parse(parser, precedence)?;

        Ok(InfixOperator::new(operator, lhs, rhs))
    }
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteral(bool);

#[cfg(test)]
impl BooleanLiteral {
    pub fn new(value: bool) -> BooleanLiteral {
        Self(value)
    }
}

impl Parsable for BooleanLiteral {
    type Output = BooleanLiteral;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        Ok(Self(parser.curr_token_is(&Token::True)))
    }
}
