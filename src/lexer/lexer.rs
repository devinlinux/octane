use std::collections::HashMap;
use crate::lexer::Token;

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    pos: usize,
    read_pos: usize,
    ch: u8,

    literal_table: HashMap<usize, String>,
    ident_idx: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into_bytes(),
            pos: 0,
            read_pos: 0,
            ch: 0,

            literal_table: HashMap::default(),
            ident_idx: 0,
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

            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            },
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            },

            b'>' => Token::GT,
            b'<' => Token::LT,

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
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    "true" => Token::True,
                    "false" => Token::False,
                    _ => {
                        self.register_literal(ident);
                        return Token::Ident(self.ident_idx - 1)
                    },
                }
            },
            b'0'..=b'9' => {
                let mut num = self.read_number();
                num = num.replace("_", "");
                let is_float = num.contains(".");

                self.register_literal(num);

                if is_float {
                    return Token::Float(self.ident_idx - 1)
                }
                return Token::Int(self.ident_idx - 1)
            }

            _ => Token::Illegal(self.ch),
        };

        self.read_char();
        tok
    }

    fn peek(&self) -> u8 {
        if self.read_pos >= self.input.len() {
            0
        } else {
            self.input[self.read_pos]
        }
    }

    fn read_ident(&mut self) -> String {
        self.read_loop(is_valid_var_char)
    }

    fn read_number(&mut self) -> String {
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

    fn register_literal(&mut self, ident: String) {
        self.literal_table.insert(self.ident_idx, ident);
        self.ident_idx += 1;
    }

    pub fn lookup_literal(&self, key: usize) -> Option<&String> {
        self.literal_table.get(&key)
    }
}

fn is_valid_var_char(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || ch == b'_'
}

fn is_valid_num_char(ch: u8) -> bool {
    ch.is_ascii_digit() || ch == b'.' || ch == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"
            let x = 5_000_000;
            let y = 3.1_4;
            if else fn
            != == = ~ ! - + < >, * /
            {}[]()
                "#;
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Token::Let,
            Token::Ident(0),
            Token::Assign,
            Token::Int(1),
            Token::Semicolon,
            Token::Let,
            Token::Ident(2),
            Token::Assign,
            Token::Float(3),
            Token::Semicolon,
            Token::If,
            Token::Else,
            Token::Function,
            Token::NotEq,
            Token::Eq,
            Token::Assign,
            Token::Illegal(b'~'),
            Token::Bang,
            Token::Minus,
            Token::Plus,
            Token::LT,
            Token::GT,
            Token::Comma,
            Token::Asterisk,
            Token::Slash,
            Token::LSquirly,
            Token::RSquirly,
            Token::LBrace,
            Token::RBrace,
            Token::LParen,
            Token::RParen,
            Token::Eof,
        ];

        for token in tokens {
            let next = lexer.next();
            //println!("expected: {token}, got: {next}");
            assert_eq!(token, next);
        }
    }
}
