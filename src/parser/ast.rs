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

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(stmt) => write!(f, "{stmt}"),
            Self::Return(stmt) => write!(f, "{stmt}"),
            Self::Expression(exp) => write!(f, "{exp}"),
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

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {{ id: {} value: {} }}", self.name, self.value)
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

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {{ value: {} }}", self.value)
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
            Token::LParen => Precedence::Call,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    BooleanLiteral(BooleanLiteral),
    FunctionLiteral(FunctionLiteral),
    PrefixOperator(PrefixOperator),
    InfixOperator(InfixOperator),
    ConditionalExpression(ConditionalExpression),
    CallExpression(CallExpression),
}

impl Expression {
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> Result<Expression, String> {
        let mut lhs = match parser.curr_token() {
            Token::Ident(_) => Identifier::parse(parser).map(Expression::Identifier),
            Token::Int(_) => IntegerLiteral::parse(parser).map(Expression::IntegerLiteral),
            Token::Float(_) => FloatLiteral::parse(parser).map(Expression::FloatLiteral),
            Token::True | Token::False => BooleanLiteral::parse(parser).map(Expression::BooleanLiteral),
            Token::Bang | Token::Minus => PrefixOperator::parse(parser).map(Expression::PrefixOperator),
            Token::LParen => Expression::parse_grouped_expression(parser),
            Token::If => ConditionalExpression::parse(parser).map(Expression::ConditionalExpression),
            Token::Function => FunctionLiteral::parse(parser).map(Expression::FunctionLiteral),
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
                Token::LParen => {
                    parser.next();
                    lhs = CallExpression::parse_with_lhs(parser, lhs).map(Expression::CallExpression)?;
                },
                _ => return Ok(lhs),
            }
        }

        Ok(lhs)
    }

    fn parse_grouped_expression(parser: &mut Parser) -> Result<Expression, String> {
        parser.next();
        let expression = Expression::parse(parser, Precedence::Lowest);

        if parser.assert_peek(&Token::RParen) {
            expression
        } else {
            Err(format!("Expected RParen, got: {}", parser.peek_token()))
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(identifier) => write!(f, "{identifier}"),
            Self::IntegerLiteral(int) => write!(f, "{int}"),
            Self::FloatLiteral(float) => write!(f, "{float}"),
            Self::BooleanLiteral(boolean) => write!(f, "{boolean}"),
            Self::FunctionLiteral(func) => write!(f, "{func}"),
            Self::PrefixOperator(prefix) => write!(f, "{prefix}"),
            Self::InfixOperator(infix) => write!(f, "{infix}"),
            Self::ConditionalExpression(conditional) => write!(f, "{conditional}"),
            Self::CallExpression(call) => write!(f, "{call}"),
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
    type Output = Identifier;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        match parser.curr_token() {
            Token::Ident(id) => Ok(Self(*id)),
            _ => Err(format!("Expected identifier, got {}", parser.curr_token())),
        }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Identifier {{ id: {} }}", self.0)
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

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IntegerLiteral {{ {} }}", self.0)
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

impl std::fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FloatLiteral {{ {} }}", self.0)
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

impl std::fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BooleanLiteral {{ {} }}", self.0)
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

impl std::fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Prefix {{ op: {} rhs: {} }}", self.operator, self.rhs)
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

impl std::fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Infix: {{ op: {}\n\tlhs: {}\n\t: rhs: {}\n}}", self.operator, self.lhs, self.rhs)
    }
}

#[derive(Debug, PartialEq)]
pub struct ConditionalExpression {
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

impl ConditionalExpression {
    pub fn new(condition: Expression, consequence: BlockStatement, alternative: Option<BlockStatement>) -> ConditionalExpression {
        Self {
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
}

impl Parsable for ConditionalExpression {
    type Output = ConditionalExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        let mut using_parens = false;
        if parser.peek_token_is(&Token::LParen) {  //  optional parens
            using_parens = true;
            parser.next();
        }
        parser.next();

        let condition = Expression::parse(parser, Precedence::Lowest)?;

        if parser.peek_token_is(&Token::RParen) {  //  optional parens
            if using_parens {
                parser.next();
            } else {
                return Err("Unexpected RParen, you did not use LParen at start of condition".to_string())
            }
        } else if using_parens {
            return Err("Expected RParen because you used LParen at start of condition".to_string())
        }

        if !parser.assert_peek(&Token::LSquirly) {
            return Err(format!("Expected RSquirly, got {}", parser.peek_token()))
        }

        let consequence = BlockStatement::parse(parser)?;

        let mut alternative = None;
        if parser.peek_token_is(&Token::Else) {
            parser.next();

            if !parser.assert_peek(&Token::LSquirly) {
                return Err(format!("Expected RSquirly, got {}", parser.peek_token()))
            }

            alternative = Some(BlockStatement::parse(parser)?);
        }

        Ok(ConditionalExpression::new(condition, consequence, alternative))
    }
}

impl std::fmt::Display for ConditionalExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Conditional: {{\n\tcondition: {}\n\tconsequence: {}\n\talternative: {:?}\n}}", self.condition, self.consequence, self.alternative)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    parameters: Vec<Identifier>,
    body: BlockStatement,
}

impl FunctionLiteral {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement) -> FunctionLiteral {
        Self {
            parameters,
            body,
        }
    }

    fn parse_function_params(parser: &mut Parser) -> Result<Vec<Identifier>, String> {
        let mut identifiers = Vec::new();

        if parser.peek_token_is(&Token::RParen) {
            parser.next();
            return Ok(identifiers);
        }

        parser.next();
        let mut ident = match parser.curr_token() {
            Token::Ident(id) => Identifier::new(*id),
            _ => return Err(format!("Expected identifier, got {}", parser.curr_token())),
        };
        identifiers.push(ident);

        while parser.peek_token_is(&Token::Comma) {
            parser.next();
            parser.next();
            ident = match parser.curr_token() {
                Token::Ident(id) => Identifier::new(*id),
                _ => return Err(format!("Expected identifier, got {}", parser.curr_token())),
            };
            identifiers.push(ident);
        }

        if !parser.assert_peek(&Token::RParen) {
            return Err(format!("Expected RParen, got {}", parser.peek_token()));
        }

        Ok(identifiers)
    }
}

impl Parsable for FunctionLiteral {
    type Output = FunctionLiteral;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        if !parser.assert_peek(&Token::LParen) {
            return Err(format!("Expected LParen, got {}", parser.peek_token()));
        }

        let params = FunctionLiteral::parse_function_params(parser)?;
        if !parser.assert_peek(&Token::LSquirly) {
            return Err(format!("Expected LSquirly, got {}", parser.peek_token()));
        }

        let body = BlockStatement::parse(parser)?;

        Ok(FunctionLiteral::new(params, body))
    }
}

impl std::fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fn: {{\n\tparams: {:?}\n\tbody: {}\n}}", self.parameters, self.body)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement(Vec<Statement>);

impl BlockStatement {
    pub fn new(statements: Vec<Statement>) -> BlockStatement {
        Self(statements)
    }
}

impl Parsable for BlockStatement {
    type Output = BlockStatement;

    fn parse(parser: &mut Parser) -> Result<Self::Output, String> {
        let mut statements = Vec::new();

        parser.next();

        while !parser.curr_token_is(&Token::RSquirly) && !parser.curr_token_is(&Token::Eof) {
            let statement = parser.parse_statement();
            if statement.is_some() {
                statements.push(statement.unwrap());
            }
            parser.next();
        }

        Ok(Self::new(statements))
    }
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::from("{\n\t\t");
        for statement in &self.0 {
            str.push_str(&format!("{}\n\t\t", statement));
        }
        str.remove(str.len() - 1);
        str.push_str("}");

        write!(f, "{str}")
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    function: Box<Expression>,
    args: Vec<Expression>,
}

impl CallExpression {
    pub fn new(function: Expression, args: Vec<Expression>) -> CallExpression {
        Self {
            function: Box::new(function),
            args,
        }
    }

    fn parse_call_args(parser: &mut Parser) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();

        if parser.peek_token_is(&Token::RParen) {
            parser.next();
            return Ok(args);
        }

        parser.next();
        args.push(Expression::parse(parser, Precedence::Lowest)?);

        while parser.peek_token_is(&Token::Comma) {
            parser.next();
            parser.next();
            args.push(Expression::parse(parser, Precedence::Lowest)?);
        }

        if !parser.assert_peek(&Token::RParen) {
            return Err(format!("Expected RParen, got {}", parser.peek_token()));
        };

        Ok(args)
    }
}

impl Parsable for CallExpression {
    type Output = CallExpression;

    fn parse(_parser: &mut Parser) -> Result<Self::Output, String> {
        unimplemented!()
    }

    fn parse_with_lhs(parser: &mut Parser, func: Expression) -> Result<Self::Output, String> {
        Ok(CallExpression::new(func, CallExpression::parse_call_args(parser)?))
    }
}

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FnCall {{\n\tfunc: {}\n\targs: {:?}", self.function, self.args)
    }
}
