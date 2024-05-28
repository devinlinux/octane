use crate::parser::ast::{ Evaluate, Program };
use crate::object::Object;

pub struct Evaluator {

}

impl Evaluator {
    pub fn eval(program: Program) -> Option<Object> {
        program.eval()
    }
}
