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
    ($parser:ident) => {{
        let Some(token) = $parser.next() else {
            let ln = $parser.ln().unwrap_or_default();
            return Err(Located::new(
                ParseError::UnexpectedEOL,
                Position::new(ln..ln + 1, 0..0),
            ));
        };
        token
    }};
    ($parser:ident : EOL) => {
        if let Some(Located { value: token, pos }) = $parser.next() {
            return Err(Located::new(ParseError::ExpectedEOL(token), pos));
        }
    };
    ($parser:ident peek) => {{
        let Some(token) = $parser.peek() else {
            let ln = $parser.ln().unwrap_or_default();
            return Err(Located::new(
                ParseError::UnexpectedEOL,
                Position::new(ln..ln + 1, 0..0),
            ));
        };
        token
    }};
    ($parser:ident : $token:ident) => {{
        let Some(Located { value: token, pos }) = $parser.next() else {
            let ln = $parser.ln().unwrap_or_default();
            return Err(Located::new(
                ParseError::UnexpectedEOL,
                Position::new(ln..ln + 1, 0..0),
            ));
        };
        if token == Token::$token {
            Located { value: token, pos }
        } else {
            return Err(Located::new(
                ParseError::ExpectedToken {
                    expected: Token::$token,
                    got: token,
                },
                pos,
            ));
        }
    }};
    ($parser:ident peek : $token:ident) => {{
        let Some(Token::$token) = $parser.peek() else {
            let ln = $parser.ln().unwrap_or_default();
            return Err(Located::new(
                ParseError::UnexpectedEOL,
                Position::new(ln..ln + 1, 0..0),
            ));
        };
        token
    }};
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
        let Located {
            value: token,
            mut pos,
        } = expected!(parser peek);
        match token {
            Token::Let => {
                expected!(parser: Let);
                let mut vars = vec![];
                while let Some(Located {
                    value: token,
                    pos: _,
                }) = parser.peek()
                {
                    if token == &Token::Equal {
                        break;
                    }
                    vars.push(Variable::parse_ident(parser)?);
                    let Located {
                        value: token,
                        pos: _,
                    } = expected!(parser peek);
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
                    let Located {
                        value: token,
                        pos: _,
                    } = expected!(parser peek);
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
                if let Some(Located {
                    value: Token::As,
                    pos: _,
                }) = parser.peek()
                {
                    expected!(parser: As);
                    ident = Some(Variable::parse_ident(parser)?);
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
                if let Some(Located {
                    value: Token::Else,
                    pos: _,
                }) = parser.peek()
                {
                    expected!(parser: Else);
                    else_case = Some(
                        if let Some(Located {
                            value: Token::If,
                            pos: _,
                        }) = parser.peek()
                        {
                            let stat = Statement::parse(parser)?;
                            let else_pos = stat.pos.clone();
                            Located::new(Block(vec![stat]), else_pos)
                        } else {
                            Block::parse(parser)?
                        },
                    );
                }
                Ok(Located::new(
                    Self::If {
                        cond,
                        case,
                        else_case,
                    },
                    pos,
                ))
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
                let var = Variable::parse_ident(parser)?;
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
                let Located { value: _, pos } = expected!(parser: Continue);
                expected!(parser: EOL);
                Ok(Located::new(Self::Break, pos))
            }
            Token::Return => {
                let Located { value: _, mut pos } = expected!(parser: Return);
                let expr = Expression::parse(parser)?;
                pos.extend(&expr.pos);
                expected!(parser: EOL);
                Ok(Located::new(Self::Return(expr), pos))
            }
            _ => {
                let assignee = Variable::parse(parser)?;
                pos.extend(&assignee.pos);
                let Located {
                    value: token,
                    pos: token_pos,
                } = expected!(parser);
                match token {
                    Token::Colon => {
                        let mut args = vec![];
                        while !parser.eol()
                        {
                            let expr = Expression::parse(parser)?;
                            pos.extend(&expr.pos);
                            args.push(expr);
                            if parser.eol() {
                                break;
                            }
                            let Located {
                                value: token,
                                pos: _,
                            } = expected!(parser peek);
                            if token != &Token::Comma {
                                break;
                            }
                            expected!(parser: Comma);
                        }
                        expected!(parser: EOL);
                        Ok(Located::new(Self::Call { var: assignee, args }, pos))
                    }
                    Token::ParanLeft => {
                        let mut args = vec![];
                        while let Some(Located {
                            value: token,
                            pos: _,
                        }) = parser.peek()
                        {
                            if token == &Token::ParanRight {
                                break;
                            }
                            args.push(Expression::parse(parser)?);
                            let Located {
                                value: token,
                                pos: _,
                            } = expected!(parser peek);
                            if token == &Token::ParanRight {
                                break;
                            }
                            if token != &Token::Comma {
                                break;
                            }
                            expected!(parser: Comma);
                        }
                        let Located {
                            value: _,
                            pos: end_pos,
                        } = expected!(parser: ParanRight);
                        pos.extend(&end_pos);
                        expected!(parser: EOL);
                        Ok(Located::new(Self::Call { var: assignee, args }, pos))
                    }
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
                            let Located {
                                value: token,
                                pos: _,
                            } = expected!(parser peek);
                            if token != &Token::Comma {
                                break;
                            }
                            expected!(parser: Comma);
                        }
                        expected!(parser: EOL);
                        Ok(Located::new(Self::Assign { idents, exprs }, pos))
                    }
                    token => {
                        return Err(Located::new(ParseError::UnexpectedToken(token), token_pos))
                    }
                }
            }
        }
    }
}
impl Parsable for Expression {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let Located {
            value: token,
            mut pos,
        } = expected!(parser peek);
        match token {
            Token::Obj => {
                expected!(parser: Obj);
                let body = Block::parse(parser)?;
                pos.extend(&body.pos);
                Ok(Located::new(Self::Obj(body), pos))
            }
            Token::Fn => {
                expected!(parser: Fn);
                let mut params = vec![];
                if let Some(Located {
                    value: Token::ParanLeft,
                    pos: _,
                }) = parser.peek()
                {
                    expected!(parser: ParanLeft);
                    while let Some(Located {
                        value: token,
                        pos: _,
                    }) = parser.peek()
                    {
                        if token == &Token::ParanRight {
                            break;
                        }
                        params.push(Variable::parse_ident(parser)?);
                        let Located {
                            value: token,
                            pos: _,
                        } = expected!(parser peek);
                        if token == &Token::ParanRight {
                            break;
                        }
                        if token != &Token::Comma {
                            break;
                        }
                        expected!(parser: Comma);
                    }
                    expected!(parser: ParanRight);
                }
                let body = Block::parse(parser)?;
                pos.extend(&body.pos);
                Ok(Located::new(Self::Fn { params, body }, pos))
            }
            _ => Self::parse_binary(parser, 0),
        }
    }
}
impl Expression {
    pub fn parse_binary(
        parser: &mut Parser,
        layer: usize,
    ) -> Result<Located<Self>, Located<ParseError>> {
        let Some(ops) = BinaryOperator::layer(layer) else {
            return Self::parse_unary(parser, 0);
        };
        let mut left = Self::parse_binary(parser, layer + 1)?;
        while let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            let Some(op) = BinaryOperator::token(token) else {
                break;
            };
            if !ops.contains(&op) {
                break;
            }
            expected!(parser);
            let right = Self::parse_binary(parser, layer + 1)?;
            let mut pos = left.pos.clone();
            pos.extend(&right.pos);
            left = Located::new(
                Self::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                pos,
            )
        }
        Ok(left)
    }
    pub fn parse_unary(
        parser: &mut Parser,
        layer: usize,
    ) -> Result<Located<Self>, Located<ParseError>> {
        let Some(ops) = UnaryOperator::layer(layer) else {
            return Self::parse_call(parser);
        };
        if let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            if let Some(op) = UnaryOperator::token(token) {
                if ops.contains(&op) {
                    let Located { value: _, mut pos } = expected!(parser);
                    let right = Self::parse_binary(parser, layer)?;
                    pos.extend(&right.pos);
                    return Ok(Located::new(
                        Self::Unary {
                            op,
                            right: Box::new(right),
                        },
                        pos,
                    ));
                }
            }
        }
        Self::parse_unary(parser, layer + 1)
    }
    pub fn parse_call(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let mut head = Atom::parse(parser)?.map(Self::Atom);
        while let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            match token {
                Token::ParanLeft => {
                    expected!(parser: ParanLeft);
                    let mut args = vec![];
                    while let Some(Located {
                        value: token,
                        pos: _,
                    }) = parser.peek()
                    {
                        if token == &Token::ParanRight {
                            break;
                        }
                        args.push(Expression::parse(parser)?);
                        let Located {
                            value: token,
                            pos: _,
                        } = expected!(parser peek);
                        if token == &Token::ParanRight {
                            break;
                        }
                        if token != &Token::Comma {
                            break;
                        }
                        expected!(parser: Comma);
                    }
                    let Located {
                        value: _,
                        pos: end_pos,
                    } = expected!(parser: ParanRight);
                    let mut pos = head.pos.clone();
                    pos.extend(&end_pos);
                    head = Located::new(
                        Self::Call {
                            head: Box::new(head),
                            args,
                        },
                        pos,
                    )
                }
                _ => break,
            }
        }
        Ok(head)
    }
}
impl BinaryOperator {
    pub const LAYER: &'static [&'static [Self]] = &[
        &[Self::Ampersand, Self::Pipe],
        &[
            Self::EqualEqual,
            Self::ExclamationEqual,
            Self::Less,
            Self::LessEqual,
            Self::Greater,
            Self::GreaterEqual,
        ],
        &[Self::Plus, Self::Minus],
        &[Self::Star, Self::Slash, Self::Percent],
        &[Self::Exponent],
    ];
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        Self::LAYER.get(layer).copied()
    }
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(Self::Plus),
            Token::Minus => Some(Self::Minus),
            Token::Star => Some(Self::Star),
            Token::Slash => Some(Self::Slash),
            Token::Percent => Some(Self::Percent),
            Token::Exponent => Some(Self::Exponent),
            Token::EqualEqual => Some(Self::EqualEqual),
            Token::ExclamationEqual => Some(Self::ExclamationEqual),
            Token::Less => Some(Self::Less),
            Token::Greater => Some(Self::Greater),
            Token::LessEqual => Some(Self::LessEqual),
            Token::GreaterEqual => Some(Self::GreaterEqual),
            Token::Ampersand => Some(Self::Ampersand),
            Token::Pipe => Some(Self::Pipe),
            _ => None,
        }
    }
}
impl UnaryOperator {
    pub const LAYER: &'static [&'static [Self]] = &[&[Self::Exclamation], &[Self::Minus]];
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        Self::LAYER.get(layer).copied()
    }
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Minus => Some(Self::Minus),
            Token::Exclamation => Some(Self::Exclamation),
            _ => None,
        }
    }
}
impl Parsable for Atom {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let Located {
            value: token,
            pos: _,
        } = expected!(parser peek);
        if token == &Token::Dot || matches!(token, Token::Ident(_)) {
            return Ok(Path::parse(parser)?.map(Self::Path));
        }
        let Located {
            value: token,
            mut pos,
        } = expected!(parser);
        match token {
            Token::Int(v) => Ok(Located::new(Self::Int(v), pos)),
            Token::Float(v) => Ok(Located::new(Self::Float(v), pos)),
            Token::Bool(v) => Ok(Located::new(Self::Bool(v), pos)),
            Token::Char(v) => Ok(Located::new(Self::Char(v), pos)),
            Token::String(v) => Ok(Located::new(Self::String(v), pos)),
            Token::ParanLeft => {
                let expr = Expression::parse(parser)?;
                let Located {
                    value: _,
                    pos: end_pos,
                } = expected!(parser: ParanRight);
                pos.extend(&end_pos);
                Ok(Located::new(Self::Expression(Box::new(expr)), pos))
            }
            Token::BracketLeft => {
                let mut exprs = vec![];
                while let Some(Located {
                    value: token,
                    pos: _,
                }) = parser.peek()
                {
                    if token == &Token::BracketRight {
                        break;
                    }
                    exprs.push(Expression::parse(parser)?);
                    let Located {
                        value: token,
                        pos: _,
                    } = expected!(parser peek);
                    if token == &Token::BracketRight {
                        break;
                    }
                    if token != &Token::Comma {
                        break;
                    }
                    expected!(parser: Comma);
                }
                let Located {
                    value: _,
                    pos: end_pos,
                } = expected!(parser: BracketRight);
                pos.extend(&end_pos);
                Ok(Located::new(Self::Vector(exprs), pos))
            }
            token => Err(Located::new(ParseError::UnexpectedToken(token), pos)),
        }
    }
}
impl Parsable for Path {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let mut head = Variable::parse(parser)?.map(Self::Variable);
        while let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            match token {
                Token::Dot => {
                    expected!(parser: Dot);
                    let field = Variable::parse_ident(parser)?;
                    let mut pos = head.pos.clone();
                    pos.extend(&field.pos);
                    head = Located::new(
                        Self::Field {
                            head: Box::new(head),
                            field,
                        },
                        pos,
                    )
                }
                Token::BracketLeft => {
                    expected!(parser: BracketLeft);
                    let index = Expression::parse(parser)?;
                    expected!(parser: BraceRight);
                    let mut pos = head.pos.clone();
                    pos.extend(&index.pos);
                    head = Located::new(
                        Self::Index {
                            head: Box::new(head),
                            index: Box::new(index),
                        },
                        pos,
                    )
                }
                _ => break,
            }
        }
        Ok(head)
    }
}
impl Parsable for Variable {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        if let Some(Located {
            value: Token::Dot,
            pos: _,
        }) = parser.peek()
        {
            let Located { value: _, mut pos } = expected!(parser: Dot);
            let Located {
                value: ident,
                pos: ident_pos,
            } = Self::parse_ident(parser)?;
            pos.extend(&ident_pos);
            return Ok(Located::new(Self::Field(ident), pos));
        }
        Ok(Self::parse_ident(parser)?.map(Self::Ident))
    }
}
impl Variable {
    pub fn parse_ident(parser: &mut Parser) -> Result<Located<String>, Located<ParseError>> {
        let Located { value: token, pos } = expected!(parser);
        if let Token::Ident(ident) = token {
            Ok(Located::new(ident, pos))
        } else {
            Err(Located::new(
                ParseError::ExpectedToken {
                    expected: Token::Ident(Default::default()),
                    got: token,
                },
                pos,
            ))
        }
    }
}

pub fn parse(text: &str) -> Result<Located<Chunk>, Located<Box<dyn Error>>> {
    let tokens = lex(text).map_err(|err| err.map(|err| err.into()))?;
    let mut parser = Parser::from(tokens);
    Chunk::parse(&mut parser).map_err(|err| err.map(|err| err.into()))
}
