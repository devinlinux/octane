mod evaluator;

use std::io::{ Write, BufRead };

const REPL_PROMPT: &str = ">> ";
const EXIT_CMD: &str = "EXIT()";

pub fn eval_repl() {
    let stdin = std::io::stdin();
    let mut stdin_lock = stdin.lock();

    loop {
        print!("{REPL_PROMPT}");
        std::io::stdout().flush().expect("Failed to flush standard out");

        let mut input = String::new();
        stdin_lock.read_line(&mut input).expect("Failed to read line");

        if input.trim() == EXIT_CMD {
            break;
        }

        let lexer = crate::lexer::Lexer::new(input);
        let mut parser = crate::parser::Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            crate::parser::print_parser_errors(parser.errors());
            continue;
        }

        let evaluated = evaluator::Evaluator::eval(program);
        println!("{:?}", evaluated);
    }
}
