use crate::parser::ast::{ Evaluate, Program };
use crate::object::Object;

pub struct Evaluator {

}

impl Evaluator {
    pub fn eval(program: Program) -> Option<Object> {
        program.eval()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_number() {
        let input = r#"
            5;
            777;
            42;
            3.14;
            "#;
        let mut evaluated_objects = Vec::new();

        for line in input.trim().lines() {
            evaluated_objects.push(evaluate_str(line));
        }

        let expected_objects = vec![
            Some(Object::Integer(5)),
            Some(Object::Integer(777)),
            Some(Object::Integer(42)),
            Some(Object::Float(3.14)),
        ];

        object_assert_loop(expected_objects, evaluated_objects);
    }

    fn evaluate_str(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors().len());

        Evaluator::eval(program)
    }

    fn object_assert_loop(expected: Vec<Option<Object>>, actual: Vec<Option<Object>>) {
        assert_eq!(expected.len(), actual.len());

        for (e, a) in expected.iter().zip(&actual) {
            match (e, a) {
                (Some(ex), Some(ac)) => assert_eq!(ex, ac),
                (None, None) => (),
                _ => panic!("Expected {:?}, got {:?}", e, a),
            }
        }
    }
}
