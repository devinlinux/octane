pub mod ast;
mod parser;

pub use parser::Parser;

use std::io::{ Write, BufRead };

const REPL_PROMPT: &str = ">> ";
const EXIT_CMD: &str = "EXIT()";

pub fn parsing_repl() {
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
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            print_parser_errors(parser.errors());
            continue;
        }

        for statement in program.statements() {
            println!("{statement}");
        }
    }
}

pub fn print_parser_errors(errors: &Vec<String>) {
    println!("Parser parsed with errors:");
    for err in errors {
        println!("\t{err}");
    }
}
