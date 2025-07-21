use crate::span::Span;

pub type Ast<'src> = Vec<Expr<'src>>;

#[derive(Debug, Clone)]
pub struct Expr<'src> {
    pub kind: ExprKind<'src>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind<'src> {
    Binary {
        left: Box<Expr<'src>>,
        op: BinOp,
        right: Box<Expr<'src>>,
    },
    Unary {
        op: BinOp,
        right: Box<Expr<'src>>,
    },
    Grouping(Box<Expr<'src>>),
    Literal(Literal<'src>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub enum Literal<'src> {
    Number(f64),
    String(&'src str),
    Nil,
    Bool(bool),
}
