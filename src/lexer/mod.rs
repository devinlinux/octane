mod token;
mod lexer;

pub use token::Token;
pub use lexer::Lexer;

use std::io::{ Write, BufRead };

const REPL_PROMPT: &str = ">> ";
const EXIT_CMD: &str = "EXIT()";

pub fn tokenization_repl() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    let mut stdin_lock = stdin.lock();

    loop {
        print!("{REPL_PROMPT}");
        std::io::stdout().flush()?;

        let mut input = String::new();
        stdin_lock.read_line(&mut input)?;

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

    Ok(())
}

pub fn tokenize_file(path: &str) -> Vec<Token> {
    let contents = crate::util::file_to_string(path);
    tokenize_string(contents)
}

pub fn tokenize_string(str: String) -> Vec<Token> {
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
