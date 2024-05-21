mod lexer;
mod util;

use clap::{ Command, Arg, ArgAction };

fn main() {
    let matches = Command::new("octane")
        .about("The OCTANE compiler")
        .version("0.1.0")
        .author("Michael Bobrowski")
        .arg(
            Arg::new("file")
            .index(1)
            .required(false)
            .help("Specifies a file to compile"),
        )
        .arg(
            Arg::new("repl")
            .short('r')
            .long("repl")
            .help("Runs a REPL"),
        )
        .get_matches();
}
