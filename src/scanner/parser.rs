use super::{
    ast::*,
    lexer::{lex, Line},
    position::{Located, Position, Spanned},
    tokens::Token,
};
use std::{
    error::Error,
    fmt::Display,
    iter::{Enumerate, Peekable},
    vec::IntoIter,
};

#[derive(Debug)]
pub struct Parser {
    pub lines: Peekable<Enumerate<IntoIter<LineParser>>>,
}
#[derive(Debug)]
pub struct LineParser {
    pub tokens: Peekable<IntoIter<Spanned<Token>>>,
    pub indent: usize,
}
impl Iterator for Parser {
    type Item = Located<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        let (ln, line) = self.lines.peek_mut()?;
        let ln = *ln;
        let token = line.next();
        if token.is_none() {
            self.lines.next();
        }
        token.map(|spanned| spanned.with_ln(ln))
    }
}
impl Iterator for LineParser {
    type Item = Spanned<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}
impl LineParser {
    pub fn peek(&mut self) -> Option<&Spanned<Token>> {
        self.tokens.peek()
    }
}
impl Parser {
    pub fn peek(&mut self) -> Option<Located<&Token>> {
        self.lines.peek_mut().and_then(|(ln, line)| {
            let Spanned { value, span } = line.peek()?;
            Some(Located::new(
                value,
                Position::new(*ln..*ln + 1, span.clone()),
            ))
        })
    }
    pub fn indent(&mut self) -> Option<usize> {
        self.lines.peek().map(|(_, line)| line.indent)
    }
    pub fn eol(&mut self) -> bool {
        self.peek().is_none()
    }
    pub fn eof(&mut self) -> bool {
        self.lines.peek().is_none()
    }
    pub fn ln(&mut self) -> Option<usize> {
        self.lines.peek().map(|(ln, _)| *ln)
    }
}
impl From<Vec<Line>> for Parser {
    fn from(value: Vec<Line>) -> Self {
        Self {
            lines: value
                .into_iter()
                .map(|line| line.into())
                .collect::<Vec<LineParser>>()
                .into_iter()
                .enumerate()
                .peekable(),
        }
    }
}
impl From<Line> for LineParser {
    fn from(value: Line) -> Self {
        Self {
            tokens: value.tokens.into_iter().peekable(),
            indent: value.indent,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedEOL,
    UnexpectedToken(Token),
    ExpectedEOL(Token),
    ExpectedToken { expected: Token, got: Token },
    ExpectedIndent { expected: usize, got: usize },
    ExpectedIndentBigger { expected: usize, got: usize },
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedEOF => write!(f, "unexpected end of file"),
            ParseError::UnexpectedEOL => write!(f, "unexpected end of line"),
            ParseError::UnexpectedToken(token) => write!(f, "unexpected token {}", token.name()),
            ParseError::ExpectedEOL(got) => write!(f, "expected end of line, got {}", got.name()),
            ParseError::ExpectedToken { expected, got } => {
                write!(f, "expected {}, got {}", expected.name(), got.name())
            }
            ParseError::ExpectedIndent { expected, got } => {
                write!(f, "expected indent of {expected}, got indent of {got}")
            }
            ParseError::ExpectedIndentBigger { expected, got } => {
                write!(
                    f,
                    "expected indent bigger than {expected}, got indent of {got}"
                )
            }
        }
    }
}
impl Error for ParseError {}
pub trait Parsable: Sized {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>>;
}
macro_rules! expected {
    ($parser:ident) => {
        {
            let Some(token) = $parser.next() else {
                let ln = $parser.ln().unwrap_or_default();
                return Err(Located::new(
                    ParseError::UnexpectedEOL,
                    Position::new(ln..ln + 1, 0..0),
                ));
            };
            token
        }
    };
    ($parser:ident : EOL) => {
        if let Some(Located { value: token, pos }) = $parser.next() {
            return Err(Located::new(ParseError::ExpectedEOL(token), pos));
        }
    };
    ($parser:ident peek) => {
        {
            let Some(token) = $parser.peek() else {
                let ln = $parser.ln().unwrap_or_default();
                return Err(Located::new(
                    ParseError::UnexpectedEOL,
                    Position::new(ln..ln + 1, 0..0),
                ));
            };
            token
        }
    };
    ($parser:ident : $token:ident) => {
        {
            let Some(Located { value: Token::$token, pos }) = $parser.next() else {
                let ln = $parser.ln().unwrap_or_default();
                return Err(Located::new(
                    ParseError::UnexpectedEOL,
                    Position::new(ln..ln + 1, 0..0),
                ));
            };
            Located { value: Token::$token, pos }
        }
    };
    ($parser:ident peek : $token:ident) => {
        {
            let Some(Token::$token) = $parser.peek() else {
                let ln = $parser.ln().unwrap_or_default();
                return Err(Located::new(
                    ParseError::UnexpectedEOL,
                    Position::new(ln..ln + 1, 0..0),
                ));
            };
            token
        }
    };
}

impl Parsable for Chunk {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let mut stats = vec![];
        let mut pos = Position::default();
        let Some(indent) = parser.indent() else {
            return Ok(Located::new(Self(stats), pos));
        };
        if indent != 0 {
            return Err(Located::new(
                ParseError::ExpectedIndent {
                    expected: 0,
                    got: indent,
                },
                pos,
            ));
        }
        while !parser.eof() {
            let stat = Statement::parse(parser)?;
            pos.extend(&stat.pos);
            stats.push(stat);
            let Some(next_line_indent) = parser.indent() else {
                continue;
            };
            if next_line_indent != indent {
                let ln = parser.ln().unwrap_or_default();
                return Err(Located::new(
                    ParseError::ExpectedIndent {
                        expected: indent,
                        got: next_line_indent,
                    },
                    Position::new(ln..ln + 1, 0..0),
                ));
            }
        }
        Ok(Located::new(Self(stats), pos))
    }
}
impl Parsable for Block {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let mut stats = vec![];
        let Some(prev_indent) = parser.indent() else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        expected!(parser: EOL);
        let Some(block_indent) = parser.indent() else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        if block_indent <= prev_indent {
            let ln = parser.ln().unwrap_or_default();
            return Err(Located::new(
                ParseError::ExpectedIndentBigger {
                    expected: prev_indent,
                    got: block_indent,
                },
                Position::new(ln..ln + 1, 0..0),
            ));
        }
        let stat = Statement::parse(parser)?;
        let mut pos = stat.pos.clone();
        stats.push(stat);
        while parser.indent() == Some(block_indent) {
            let stat = Statement::parse(parser)?;
            pos.extend(&stat.pos);
            stats.push(stat);
            let Some(next_line_indent) = parser.indent() else {
                return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
            };
            if next_line_indent != block_indent {
                let ln = parser.ln().unwrap_or_default();
                return Err(Located::new(
                    ParseError::ExpectedIndent {
                        expected: block_indent,
                        got: next_line_indent,
                    },
                    Position::new(ln..ln + 1, 0..0),
                ));
            }
        }
        Ok(Located::new(Self(stats), pos))
    }
}
impl Parsable for Statement {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let Located { value: token, mut pos } = expected!(parser peek);
        match token {
            Token::Let => {
                expected!(parser: Let);
                let mut vars = vec![];
                while let Some(Located { value: token, pos: _ }) = parser.peek() {
                    if token == &Token::Equal {
                        break;
                    }
                    vars.push(Path::parse_ident(parser)?);
                    let Located { value: token, pos: _ } = expected!(parser peek);
                    if token == &Token::Equal {
                        break;
                    }
                    if token != &Token::Comma {
                        break;
                    }
                    expected!(parser: Comma);
                }
                expected!(parser: Equal);
                let mut exprs = vec![];
                while !parser.eol() {
                    let expr = Expression::parse(parser)?;
                    pos.extend(&expr.pos);
                    exprs.push(expr);
                    if parser.eol() {
                        break;
                    }
                    let Located { value: token, pos: _ } = expected!(parser peek);
                    if token != &Token::Comma {
                        break;
                    }
                    expected!(parser: Comma);
                }
                expected!(parser: EOL);
                Ok(Located::new(Self::Let { vars, exprs }, pos))
            }
            Token::With => {
                expected!(parser: With);
                let expr = Expression::parse(parser)?;
                let mut ident = None;
                if let Some(Located { value: Token::As, pos: _ }) = parser.peek() {
                    expected!(parser: As);
                    ident = Some(Path::parse_ident(parser)?);
                }
                let body = Block::parse(parser)?;
                Ok(Located::new(Self::With { expr, ident, body }, pos))
            }
            Token::Do => {
                expected!(parser: Do);
                let body = Block::parse(parser)?;
                Ok(Located::new(Self::Do(body), pos))
            }
            Token::If => {
                expected!(parser: If);
                let cond = Expression::parse(parser)?;
                let case = Block::parse(parser)?;
                let mut else_case = None;
                if let Some(Located { value: Token::Else, pos: _ }) = parser.peek() {
                    expected!(parser: Else);
                    else_case = Some(if let Some(Located { value: Token::If, pos: _ }) = parser.peek() {
                        let stat = Statement::parse(parser)?;
                        let else_pos = stat.pos.clone();
                        Located::new(Block(vec![stat]), else_pos)
                    } else {
                        Block::parse(parser)?
                    });
                }
                Ok(Located::new(Self::If { cond, case, else_case }, pos))
            }
            Token::Loop => {
                expected!(parser: Loop);
                let body = Block::parse(parser)?;
                Ok(Located::new(Self::Loop(body), pos))
            }
            Token::While => {
                expected!(parser: While);
                let cond = Expression::parse(parser)?;
                let body = Block::parse(parser)?;
                Ok(Located::new(Self::While { cond, body }, pos))
            }
            Token::Repeat => {
                expected!(parser: Repeat);
                let body = Block::parse(parser)?;
                expected!(parser: Until);
                let cond = Expression::parse(parser)?;
                expected!(parser: EOL);
                Ok(Located::new(Self::Repeat { body, cond }, pos))
            }
            Token::For => {
                expected!(parser: For);
                let var = Path::parse_ident(parser)?;
                expected!(parser: In);
                let iter = Expression::parse(parser)?;
                let body = Block::parse(parser)?;
                Ok(Located::new(Self::For { var, iter, body }, pos))
            }
            Token::Break => {
                let Located { value: _, pos } = expected!(parser: Break);
                expected!(parser: EOL);
                Ok(Located::new(Self::Break, pos))
            }
            Token::Continue => {
                let Located { value: _, pos } = expected!(parser: Break);
                expected!(parser: EOL);
                Ok(Located::new(Self::Break, pos))
            }
            Token::Return => {
                let Located { value: _, mut pos } = expected!(parser: Break);
                let expr = Expression::parse(parser)?;
                pos.extend(&expr.pos);
                expected!(parser: EOL);
                Ok(Located::new(Self::Return(expr), pos))
            }
            _ => {
                let assignee = Assignee::parse(parser)?;
                pos.extend(&assignee.pos);
                let Located { value: token, pos: token_pos } = expected!(parser);
                match token {
                    Token::Equal => {
                        let idents = vec![assignee];
                        let mut exprs = vec![];
                        while !parser.eol() {
                            let expr = Expression::parse(parser)?;
                            pos.extend(&expr.pos);
                            exprs.push(expr);
                            if parser.eol() {
                                break;
                            }
                            let Located { value: token, pos: _ } = expected!(parser peek);
                            if token != &Token::Comma {
                                break;
                            }
                            expected!(parser: Comma);
                        }
                        expected!(parser: EOL);
                        Ok(Located::new(Self::Assign { idents, exprs }, pos))
                    }
                    token => return Err(Located::new(ParseError::UnexpectedToken(token), token_pos))
                }
            }
        }
    }
}
impl Parsable for Assignee {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        if let Some(Located { value: Token::Dot, pos: _ }) = parser.peek() {
            let Located { value: _, mut pos } = expected!(parser: Dot);
            let Located { value: ident, pos: ident_pos } = Path::parse_ident(parser)?;
            pos.extend(&ident_pos);
            return Ok(Located::new(Self::Field(ident), pos))
        }
        Ok(Path::parse_ident(parser)?.map(Self::Ident))
    }
}
impl Parsable for Expression {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        Ok(Atom::parse(parser)?.map(Self::Atom))
    }
}
impl Parsable for Atom {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let Located { value: token, pos: _ } = expected!(parser peek);
        if token == &Token::Dot || matches!(token, Token::Ident(_)) {
            return Ok(Path::parse(parser)?.map(Self::Path))
        }
        let Located { value: token, mut pos } = expected!(parser);
        match token {
            Token::Int(v) => Ok(Located::new(Self::Int(v), pos)),
            Token::Float(v) => Ok(Located::new(Self::Float(v), pos)),
            Token::Bool(v) => Ok(Located::new(Self::Bool(v), pos)),
            Token::Char(v) => Ok(Located::new(Self::Char(v), pos)),
            Token::String(v) => Ok(Located::new(Self::String(v), pos)),
            Token::ParanLeft => {
                let expr = Expression::parse(parser)?;
                let Located { value: _, pos: end_pos } = expected!(parser: ParanRight);
                pos.extend(&end_pos);
                Ok(Located::new(Self::Expression(Box::new(expr)), pos))
            }
            // Token::BracketLeft => {
            //     let expr = Expression::parse(parser)?;
            //     let Located { value: _, pos: end_pos } = expected!(parser: BracketRight);
            //     pos.extend(&end_pos);
            //     Ok(Located::new(Self::Expression(Box::new(expr)), pos))
            // }
            token => Err(Located::new(ParseError::UnexpectedToken(token), pos))
        }
    }
}
impl Parsable for Path {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        todo!()
    }
}
impl Path {
    pub fn parse_ident(parser: &mut Parser) -> Result<Located<String>, Located<ParseError>> {
        let Located { value: token, pos } = expected!(parser);
        if let Token::Ident(ident) = token {
            Ok(Located::new(ident, pos))
        } else {
            Err(Located::new(ParseError::ExpectedToken { expected: Token::Ident(Default::default()), got: token }, pos))
        }
    }
}

pub fn parse(text: &str) -> Result<Located<Chunk>, Located<Box<dyn Error>>> {
    let tokens = lex(text).map_err(|err| err.map(|err| err.into()))?;
    let mut parser = Parser::from(tokens);
    Chunk::parse(&mut parser).map_err(|err| err.map(|err| err.into()))
}
