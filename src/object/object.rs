use std::ops::{ Add, Div, Mul, Sub };
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Skip,
    Error(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Return(Box<Object>),
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
