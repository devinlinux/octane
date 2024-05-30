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

    #[test]
    fn test_eval_prefix() {
        let input = r#"
            !true
            !false
            -3.14
            --777
            "#;
        let mut evaluated_objects = Vec::new();
        for line in input.trim().lines() {
            evaluated_objects.push(evaluate_str(line))
        }

        let expected_objects = vec![
            Object::Boolean(false),
            Object::Boolean(true),
            Object::Float(-3.14),
            Object::Integer(777),
        ];

        object_assert_loop(expected_objects, evaluated_objects)
    }

    #[test]
    fn test_eval_infix() {
        let input = r#"
            2 + 2;
            5.1 + 5;
            5 == 5
            1 != 1
            (5 + 5) - 2 * 3;
            "#;
        let mut evaluated_objects = Vec::new();
        for line in input.trim().lines() {
            evaluated_objects.push(evaluate_str(line))
        }

        let expected_objects = vec![
            Object::Integer(4),
            Object::Float(10.1),
            Object::Boolean(true),
            Object::Boolean(false),
            Object::Integer(4),
        ];

        object_assert_loop(expected_objects, evaluated_objects)
    }

    #[test]
    fn test_eval_conditional() {
        let input = r#"
            if (false) { 5; }
            if (true) { 7; } else { 6; }
            if (false) { 6; } else { 7; }
            "#;
        let mut evaluated_objects = Vec::new();
        for line in input.trim().lines() {
            evaluated_objects.push(evaluate_str(line))
        }

        let expected_objects = vec![
            Object::Skip,
            Object::Integer(7),
            Object::Integer(7),
        ];

        object_assert_loop(expected_objects, evaluated_objects)
    }

    #[test]
    fn test_eval_return() {
        let input = r#"
            return 5;
            return true;
            "#;
        let mut evaluated_objects = Vec::new();
        for line in input.trim().lines() {
            evaluated_objects.push(evaluate_str(line))
        }

        let expected_objects = vec![
            Object::Return(Box::new(Object::Integer(5))),
            Object::Return(Box::new(Object::Boolean(true))),
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
