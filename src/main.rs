mod util;
mod lexer;
mod parser;
mod object;
mod interpreter;

use std::env;

fn main() {
    if let Err(e)  = _main() {
        eprintln!("Error: {e:#?}");
        std::process::exit(1);
    }
}

fn _main<'a>() -> Result<(), &'a str> {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => Err("Usage octane <file path> | octane --rlpl | octane -rppl | octane --repl"),
        _ => match args[1].as_str() {
            "--rlpl" => {
                lexer::tokenization_repl();
                Ok(())
            },
            "--rppl" => {
                parser::parsing_repl();
                Ok(())
            },
            "--repl" => {
                interpreter::eval_repl();
                Ok(())
            },
            _ => {
                lexer::tokenize_file(&args[1]).iter().for_each(|token| println!("{token}"));
                Ok(())
            }
        }
    }
}
