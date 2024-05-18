#[derive(PartialEq)]
pub enum Token {
    Illegal(u8),
    Eof,

    //  identifiers + literals
    Ident(String),
    Int(i64),
    Float(f64),

    //  Operators
    Assign,
    Plus,
    Dash,
    Asterisk,
    Slash,
    Bang,

    //  Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquirly,
    RSquirly,

    //  Keywords
    Function,
    Let,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal(ch) => write!(f, "Illegal( {} )", ch),
            Token::Eof => write!(f, "Eof"),

            Token::Ident(ident) => write!(f, "Ident( {} )", ident),
            Token::Int(int) => write!(f, "Int( {} )", int),
            Token::Float(float) => write!(f, "Float( {} )", float),

            Token::Assign => write!(f, "Assign"),
            Token::Plus => write!(f, "Plus"),
            Token::Dash => write!(f, "Dash"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Slash => write!(f, "Slash"),
            Token::Bang => write!(f, "Bang"),

            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),

            Token::LParen => write!(f, "LParen"),
            Token::RParen => write!(f, "RParen"),
            Token::LBrace => write!(f, "LBrace"),
            Token::RBrace => write!(f, "RBrace"),
            Token::LSquirly => write!(f, "LSquirly"),
            Token::RSquirly => write!(f, "RSquirly"),

            Token::Function => write!(f, "Function"),
            Token::Let => write!(f, "Let"),
        }
    }
}
