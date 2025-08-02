use crate::{
    expr::{Expr, Identifier},
    id::NodeId,
    span::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt<'src> {
    pub kind: StmtKind<'src>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind<'src> {
    Expression(Expr<'src>),
    Print(Expr<'src>),
    Var {
        name: Identifier<'src>,
        init: Option<Expr<'src>>,
    },
    Block(Vec<Stmt<'src>>),
    If {
        condition: Expr<'src>,
        then_branch: Box<Stmt<'src>>,
        else_branch: Option<Box<Stmt<'src>>>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    Function(FunctionDecl<'src>),
    Return(Option<Expr<'src>>),
    Class(ClassDecl<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl<'src> {
    pub name: Identifier<'src>,
    pub params: Vec<Identifier<'src>>,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl<'src> {
    pub name: Identifier<'src>,
    pub methods: Vec<FunctionDecl<'src>>,
}
