mod token;
mod lexer;

pub use token::Token;
pub use lexer::Lexer;

use std::io::{ Write, BufRead };

const REPL_PROMPT: &str = ">> ";
const EXIT_CMD: &str = "EXIT()";

pub fn tokenization_repl() {
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

        let mut lexer = Lexer::new(input);

        loop {
            let token = lexer.next();
            println!("{token}");

            if let Token::Eof = token {
                break;
            }
        }
    }
}

pub fn tokenize_file(path: &str) -> Vec<Token> {
    let contents = crate::util::file_to_string(path);
    tokenize_string(contents)
}

fn tokenize_string(str: String) -> Vec<Token> {
    let mut lexer = Lexer::new(str);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next();
        tokens.push(token);

        if let Some(Token::Eof) = tokens.last() {
            break;
        }
    }

    tokens
}
