use super::position::Located;

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        vars: Vec<Located<String>>,
        exprs: Vec<Located<Expression>>,
    },
    Assign {
        idents: Vec<Located<Assignee>>,
        exprs: Vec<Located<Expression>>,
    },
    With {
        expr: Located<Expression>,
        ident: Option<Located<String>>,
        body: Located<Block>,
    },

    Do(Located<Block>),
    If {
        cond: Located<Expression>,
        case: Located<Block>,
        else_case: Option<Located<Block>>,
    },

    Loop(Located<Block>),
    While {
        cond: Located<Expression>,
        body: Located<Block>,
    },
    Repeat {
        body: Located<Block>,
        cond: Located<Expression>,
    },
    For {
        var: Located<String>,
        iter: Located<Expression>,
        body: Located<Block>,
    },

    Break,
    Continue,
    Return(Located<Expression>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Assignee {
    Ident(String),
    Field(String),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    Binary {
        op: BinaryOperator,
        left: Box<Located<Self>>,
        right: Box<Located<Self>>,
    },
    Unary {
        op: UnaryOperator,
        right: Box<Located<Self>>,
    },
    Call {
        head: Located<Atom>,
        args: Vec<Located<Self>>,
    },
    Obj(Block),
    If {
        cond: Box<Located<Self>>,
        case: Located<Block>,
        else_case: Option<Located<Block>>,
    },
    Fn {
        params: Vec<Located<String>>,
        body: Located<Block>,
    },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exponent,
    EqualEqual,
    ExclamationEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Ampersand,
    Pipe,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
    Exclamation,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Path(Path),
    Int(isize),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Expression(Box<Located<Expression>>),
    Vector(Vec<Located<Expression>>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    Ident(String),
    Field {
        head: Box<Located<Self>>,
        field: Located<String>,
    },
    Index {
        head: Box<Located<Self>>,
        index: Box<Located<Expression>>,
    },
}
