use super::position::Located;

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Block(Vec<Located<Statement>>);
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
    Do(Block),
    If {
        cond: Located<Expression>,
        case: Located<Block>,
        else_case: Option<Located<Block>>,
    },
    While {
        cond: Located<Expression>,
        body: Located<Block>,
    },
    Repeat {
        body: Located<Block>,
        cond: Located<Expression>,
    },
    For {
        vars: Located<String>,
        iter: Located<Expression>,
        body: Located<Block>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub enum Assignee {
    Ident(String),
    Field(String),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {}
#[derive(Debug, Clone, PartialEq)]
pub enum Path {}
