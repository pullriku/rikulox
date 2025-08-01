use crate::{
    expr::{Expr, Identifier},
    id::NodeId,
    span::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expression(Expr),
    Print(Expr),
    Var {
        name: Identifier,
        init: Option<Expr>,
    },
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function(FunctionDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Vec<Stmt>,
}
