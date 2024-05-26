#[derive(PartialEq, Debug)]
pub enum Token {
    Illegal(u8),
    Eof,

    //  identifiers + literals
    Ident(u32),
    Int(i64),
    Float(f64),

    //  Operators
    Assign,
    Plus,
    Dash,
    Asterisk,
    Slash,
    Bang,

    GT,
    LT,
    Eq,
    NotEq,

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
    If,
    Else,
    Return,
}

impl Default for Token {
    fn default() -> Token {
        Token::Illegal(0)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            //  identifiers + literals
            Token::Illegal(byte) => write!(f, "Illegal( {} )", *byte as char),
            Token::Eof => write!(f, "Eof"),

            Token::Ident(ident) => write!(f, "Ident( {} )", ident),
            Token::Int(int) => write!(f, "Int( {} )", int),
            Token::Float(float) => write!(f, "Float( {} )", float),

            //  Operators
            Token::Assign => write!(f, "Assign"),
            Token::Plus => write!(f, "Plus"),
            Token::Dash => write!(f, "Dash"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Slash => write!(f, "Slash"),
            Token::Bang => write!(f, "Bang"),

            Token::GT => write!(f, "Greater than"),
            Token::LT => write!(f, "Less than"),
            Token::Eq => write!(f, "Equal"),
            Token::NotEq => write!(f, "Not Equal"),

            //  Delimiters
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),

            Token::LParen => write!(f, "LParen"),
            Token::RParen => write!(f, "RParen"),
            Token::LBrace => write!(f, "LBrace"),
            Token::RBrace => write!(f, "RBrace"),
            Token::LSquirly => write!(f, "LSquirly"),
            Token::RSquirly => write!(f, "RSquirly"),

            //  Keywords
            Token::Function => write!(f, "Function"),
            Token::Let => write!(f, "Let"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Return => write!(f, "Return"),
        }
    }
}
