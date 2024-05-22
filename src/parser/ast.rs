use crate::lexer::Token;

pub struct Program<'a> {
    statements: Vec<Statement<'a>>,
}

pub enum Statement<'a> {
    Let(LetStatement<'a>),
}

pub enum Expression {

}

pub struct Identifier<'a> {
    token: Token,
    value: &'a str,
}

pub struct LetStatement<'a> {
    name: Identifier<'a>,
    value: Expression,
}
