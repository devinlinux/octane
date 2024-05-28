use crate::parser::ast::{ Evaluate, Program };
use crate::object::Object;

pub struct Evaluator {

}

impl Evaluator {
    pub fn eval(program: Program) -> Object {
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
            Object::Integer(5),
            Object::Integer(777),
            Object::Integer(42),
            Object::Float(3.14),
        ];

        object_assert_loop(expected_objects, evaluated_objects);
    }

    #[test]
    fn test_eval_boolean() {
        let input = r#"
            true;
            false;
            "#;
        let mut evaluated_objects = Vec::new();
        for line in input.trim().lines() {
            evaluated_objects.push(evaluate_str(line))
        }

        let expected_objects = vec![
            Object::Boolean(true),
            Object::Boolean(false),
        ];

        object_assert_loop(expected_objects, evaluated_objects)
    }

    fn evaluate_str(input: &str) -> Object {
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(0, parser.errors().len());

        Evaluator::eval(program)
    }

    fn object_assert_loop(expected: Vec<Object>, actual: Vec<Object>) {
        assert_eq!(expected.len(), actual.len());

        for (e, a) in expected.iter().zip(&actual) {
            assert_eq!(e, a);
        }
    }
}
