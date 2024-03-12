use std::{
    fmt::Display, iter::{Enumerate, Peekable}, num::{ParseFloatError, ParseIntError}, str::{Chars, Lines}
};

use super::{
    position::{Located, Position, Spanned},
    tokens::Token,
};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    pub lines: Enumerate<Lines<'a>>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Line {
    pub tokens: Vec<Spanned<Token>>,
    pub indent: usize,
    pub ln: usize,
}
#[derive(Debug, Clone)]
pub struct LineLexer<'a> {
    chars: Peekable<Enumerate<Chars<'a>>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    BadCharacter(char),
    UnclosedCharacter,
    ExpectedCharacter,
    UnclosedString,
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Line, Located<LexError>>;
    fn next(&mut self) -> Option<Self::Item> {
        let (ln, line) = self.lines.next()?;
        let mut indent = 0;
        let mut chars = line.chars().enumerate().peekable();
        while let Some((_, c)) = chars.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }
            indent += 1;
            chars.next();
        }
        let mut line_lexer = LineLexer { chars };
        Some(Ok(Line {
            tokens: {
                let mut tokens = vec![];
                while let Some(res) = line_lexer.next() {
                    match res {
                        Ok(token) => tokens.push(token),
                        Err(Spanned { value: err, span }) => {
                            return Some(Err(Located::new(err, Position::new(ln..ln + 1, span))))
                        }
                    }
                }
                tokens
            },
            indent,
            ln,
        }))
    }
}
impl<'a> Iterator for LineLexer<'a> {
    type Item = Result<Spanned<Token>, Spanned<LexError>>;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((_, c)) = self.chars.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }
            self.chars.next();
        }
        let (col, c) = self.chars.next()?;
        let mut span = col..col + 1;
        match c {
            '=' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::EqualEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Equal, span)))
                }
            }
            ',' => Some(Ok(Spanned::new(Token::Comma, span))),
            '.' => Some(Ok(Spanned::new(Token::Dot, span))),
            ':' => Some(Ok(Spanned::new(Token::Colon, span))),
            '(' => Some(Ok(Spanned::new(Token::ParanLeft, span))),
            ')' => Some(Ok(Spanned::new(Token::ParanRight, span))),
            '[' => Some(Ok(Spanned::new(Token::BracketLeft, span))),
            ']' => Some(Ok(Spanned::new(Token::BracketRight, span))),
            '{' => Some(Ok(Spanned::new(Token::BraceLeft, span))),
            '}' => Some(Ok(Spanned::new(Token::BraceRight, span))),
            '+' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::PlusEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Plus, span)))
                }
            }
            '-' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::MinusEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Minus, span)))
                }
            }
            '*' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::StarEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Star, span)))
                }
            }
            '/' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::SlashEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Slash, span)))
                }
            }
            '%' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::PercentEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Percent, span)))
                }
            }
            '^' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::ExponentEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Exponent, span)))
                }
            }
            '<' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::LessEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Less, span)))
                }
            }
            '>' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::GreaterEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Greater, span)))
                }
            }
            '!' => {
                if let Some((col, '=')) = self.chars.peek().copied() {
                    self.chars.next();
                    span.end = col + 1;
                    Some(Ok(Spanned::new(Token::ExclamationEqual, span)))
                } else {
                    Some(Ok(Spanned::new(Token::Exclamation, span)))
                }
            }
            '&' => Some(Ok(Spanned::new(Token::Ampersand, span))),
            '|' => Some(Ok(Spanned::new(Token::Pipe, span))),
            '\'' => {
                let Some((_, c)) = self.chars.next() else {
                    return Some(Err(Spanned::new(LexError::ExpectedCharacter, span)));
                };
                let c = if c == '\\' {
                    let Some((_, c)) = self.chars.next() else {
                        return Some(Err(Spanned::new(LexError::ExpectedCharacter, span)));
                    };
                    match c {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '0' => '\0',
                        c => c,
                    }
                } else {
                    c
                };
                let Some((end, '\'')) = self.chars.next() else {
                    return Some(Err(Spanned::new(LexError::UnclosedCharacter, span)));
                };
                span.end = end + 1;
                Some(Ok(Spanned::new(Token::Char(c), span)))
            }
            '"' => {
                let mut string = String::new();
                while let Some((_, c)) = self.chars.peek() {
                    if *c == '"' {
                        break;
                    }
                    string.push(if *c == '\\' {
                        let Some((_, c)) = self.chars.next() else {
                            return Some(Err(Spanned::new(LexError::ExpectedCharacter, span)));
                        };
                        match c {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '0' => '\0',
                            c => c,
                        }
                    } else {
                        *c
                    });
                    self.chars.next();
                }
                let Some((end, '"')) = self.chars.next() else {
                    return Some(Err(Spanned::new(LexError::UnclosedString, span)));
                };
                span.end = end + 1;
                Some(Ok(Spanned::new(Token::String(string), span)))
            }
            c if c.is_ascii_digit() => {
                let mut number = String::from(c);
                while let Some((col, c)) = self.chars.peek() {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    number.push(*c);
                    span.end = col + 1;
                    self.chars.next();
                }
                if let Some((col, '.')) = self.chars.peek() {
                    number.push('.');
                    span.end = *col + 1;
                    self.chars.next();
                    while let Some((col, c)) = self.chars.peek() {
                        if !c.is_ascii_alphanumeric() {
                            break;
                        }
                        number.push(*c);
                        span.end = col + 1;
                        self.chars.next();
                    }
                    Some(Ok(Spanned::new(
                        Token::Float(match number.parse() {
                            Ok(number) => number,
                            Err(err) => {
                                return Some(Err(Spanned::new(
                                    LexError::ParseFloatError(err),
                                    span,
                                )))
                            }
                        }),
                        span,
                    )))
                } else {
                    Some(Ok(Spanned::new(
                        Token::Int(match number.parse() {
                            Ok(number) => number,
                            Err(err) => {
                                return Some(Err(Spanned::new(LexError::ParseIntError(err), span)))
                            }
                        }),
                        span,
                    )))
                }
            }
            c if c.is_ascii_alphanumeric() || c == '_' => {
                let mut ident = String::from(c);
                while let Some((col, c)) = self.chars.peek() {
                    if !c.is_ascii_alphanumeric() && *c != '_' {
                        break;
                    }
                    ident.push(*c);
                    span.end = col + 1;
                    self.chars.next();
                }
                Some(Ok(Spanned::new(Token::Ident(ident), span)))
            }
            c => Some(Err(Spanned::new(LexError::BadCharacter(c), span))),
        }
    }
}
impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::BadCharacter(c) => write!(f, "bad character {c:?}"),
            LexError::UnclosedCharacter => write!(f, "unclosed character"),
            LexError::ExpectedCharacter => write!(f, "expected character"),
            LexError::UnclosedString => write!(f, "unclosed string"),
            LexError::ParseIntError(err) => write!(f, "error while parsing int: {err}"),
            LexError::ParseFloatError(err) => write!(f, "error while parsing float: {err}"),
        }
    }
}

pub fn lex(text: &str) -> Result<Vec<Line>, Located<LexError>> {
    let mut lines = vec![];
    let mut lexer = Lexer {
        lines: text.lines().enumerate(),
    };
    while let Some(res) = lexer.next() {
        lines.push(res?);
    }
    Ok(lines)
}
