use std::ops::{ Add, Div, Mul, Sub };
use std::cmp::Ordering;
use crate::parser::ast::{ Identifier, BlockStatement };

use super::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Skip,
    Error(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Return(Box<Object>),
    Function(Function),
}

impl Add for Object {
    type Output = Object;

    fn add(self, other: Object) -> Object {
        match (self, other) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs + rhs),
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs + rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => Object::Float(lhs as f64 + rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => Object::Float(lhs + rhs as f64),
            _ => unimplemented!(),
        }
    }
}

impl Sub for Object {
    type Output = Object;

    fn sub(self, other: Object) -> Object {
        match (self, other) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs - rhs),
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs - rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => Object::Float(lhs as f64 - rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => Object::Float(lhs - rhs as f64),
            _ => unimplemented!(),
        }
    }
}

impl Mul for Object {
    type Output = Object;

    fn mul(self, other: Object) -> Object {
        match (self, other) {
            (Object::Integer(lhs), Object::Integer(rhs)) => Object::Integer(lhs * rhs),
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs * rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => Object::Float(lhs as f64 * rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => Object::Float(lhs * rhs as f64),
            _ => unimplemented!(),
        }
    }
}

impl Div for Object {
    type Output = Object;

    fn div(self, other: Object) -> Object {
        match (self, other) {
            (Object::Integer(lhs), Object::Integer(rhs)) => {
                if rhs != 0 {
                    Object::Integer(lhs / rhs)
                } else {
                    Object::Error(String::from("Division by zero"))
                }
            }
            (Object::Float(lhs), Object::Float(rhs)) => Object::Float(lhs / rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => Object::Float(lhs as f64 / rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => Object::Float(lhs / rhs as f64),
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Object) -> Option<Ordering> {
        match (self, other) {
            (Object::Integer(lhs), Object::Integer(rhs)) => lhs.partial_cmp(rhs),
            (Object::Float(lhs), Object::Float(rhs)) => lhs.partial_cmp(rhs),
            (Object::Integer(lhs), Object::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Object::Float(lhs), Object::Integer(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    parameters: Vec<Identifier>,
    body: BlockStatement,
    env: Environment,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Environment) -> Function {
        Self {
            parameters,
            body,
            env,
        }
    }

    pub fn parameters(&self) -> &[Identifier] {
        &self.parameters
    }

    pub fn body(&self) -> &BlockStatement {
        &self.body
    }

    pub fn env(&self) -> Environment {
        self.env.clone()
    }
}
