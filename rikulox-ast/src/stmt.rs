use crate::{
    expr::{Expr, Identifier},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expression(Expr),
    Print(Expr),
    Var {
        name: Identifier,
        init: Option<Expr>,
    },
    Block(Vec<Stmt>),
}
