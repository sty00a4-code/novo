use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),

    Equal,
    Comma,
    Dot,
    Colon,
    
    ParanLeft,
    ParanRight,
    BracketLeft,
    BracketRight,
    BraceLeft,
    BraceRight,
    
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exponent,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    ExponentEqual,
    
    EqualEqual,
    ExclamationEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Exclamation,
    Ampersand,
    Pipe,

    Let,
    Fn,
    Obj,
    If,
    Else,
    While,
    Repeat,
    Until,
    For,
    In,
}

impl Token {
    pub fn name(&self) -> String {
        match self {
            Self::Ident(_) => "<ident>".to_string(),
            Self::Int(_) => "<int>".to_string(),
            Self::Float(_) => "<float>".to_string(),
            Self::Bool(_) => "<bool>".to_string(),
            Self::Char(_) => "<char>".to_string(),
            Self::String(_) => "<string>".to_string(),
            token => format!("{:?}", token.to_string())
        }
    }
    pub fn ident(ident: String) -> Self {
        match ident.as_str() {
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            "let" => Self::Let,
            "fn" => Self::Fn,
            "obj" => Self::Obj,
            "if" => Self::If,
            "else" => Self::Else,
            "while" => Self::While,
            "repeat" => Self::Repeat,
            "until" => Self::Until,
            "for" => Self::For,
            "in" => Self::In,
            _ => Self::Ident(ident)
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Float(float) => write!(f, "{float:?}"),
            Self::Bool(bool) => write!(f, "{bool:?}"),
            Self::Char(char) => write!(f, "{char:?}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::Equal => write!(f, "="),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            Self::ParanLeft => write!(f, "("),
            Self::ParanRight => write!(f, ")"),
            Self::BracketLeft => write!(f, "["),
            Self::BracketRight => write!(f, "]"),
            Self::BraceLeft => write!(f, "{{"),
            Self::BraceRight => write!(f, "}}"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Exponent => write!(f, "^"),
            Self::PlusEqual => write!(f, "+="),
            Self::MinusEqual => write!(f, "-="),
            Self::StarEqual => write!(f, "*="),
            Self::SlashEqual => write!(f, "/="),
            Self::PercentEqual => write!(f, "%="),
            Self::ExponentEqual => write!(f, "^="),
            Self::EqualEqual => write!(f, "=="),
            Self::ExclamationEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::LessEqual => write!(f, "<="),
            Self::GreaterEqual => write!(f, ">="),
            Self::Exclamation => write!(f, "!="),
            Self::Ampersand => write!(f, "&"),
            Self::Pipe => write!(f, "|"),
            Self::Let => write!(f, "let"),
            Self::Fn => write!(f, "fn"),
            Self::Obj => write!(f, "obj"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
            Self::Repeat => write!(f, "repeat"),
            Self::Until => write!(f, "until"),
            Self::For => write!(f, "for"),
            Self::In => write!(f, "in"),
        }
    }
}