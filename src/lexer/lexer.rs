use crate::lexer::Token;

pub struct Lexer {
    input: Vec<u8>,
    pos: usize,
    read_pos: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into_bytes(),
            pos: 0,
            read_pos: 0,
            ch: 0,
        };
        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_pos]
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn eat_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next(&mut self) -> Token {
        self.eat_whitespace();

        let tok = match self.ch {
            0 => Token::Eof,

            b'=' => Token::Assign,
            b'+' => Token::Plus,
            b'-' => Token::Dash,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'!' => Token::Bang,

            b',' => Token::Comma,
            b';' => Token::Semicolon,

            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'[' => Token::LBrace,
            b']' => Token::RBrace,
            b'{' => Token::LSquirly,
            b'}' => Token::RSquirly,

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                match ident.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    _ => Token::Ident(ident),
                }
            },
            b'0'..=b'9' => {
                let mut num = self.read_num();
                num = num.replace("_", "");
                if num.contains(".") {
                    Token::Float(num.parse::<f64>().unwrap())
                } else {
                    Token::Int(num.parse::<i64>().unwrap())
                }
            }

            _ => Token::Illegal(self.ch),
        };

        self.read_char();
        tok
    }

    fn read_ident(&mut self) -> String {
        self.read_loop(is_valid_var_char)
    }

    fn read_num(&mut self) -> String {
        self.read_loop(is_valid_num_char)
    }

    fn read_loop<F>(&mut self, condition: F) -> String
    where 
        F: Fn(u8) -> bool
    {
        let start = self.pos;
        while condition(self.ch) {
            self.read_char()
        }
        String::from_utf8_lossy(&self.input[start..self.pos]).to_string()
    }
}

fn is_valid_var_char(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || ch == b'_'
}

fn is_valid_num_char(ch: u8) -> bool {
    ch.is_ascii_digit() || ch == b'.' || ch == b'_'
}
