use crate::{
    id::NodeId,
    span::Span,
    token::{Keyword, TokenKind},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'src> {
    pub kind: ExprKind<'src>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'src> {
    Binary {
        left: Box<Expr<'src>>,
        op: BinOp,
        right: Box<Expr<'src>>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr<'src>>,
    },
    Grouping(Box<Expr<'src>>),
    Literal(Literal<'src>),
    Variable(Variable<'src>),
    Assign {
        name: Identifier<'src>,
        value: Box<Expr<'src>>,
    },
    Logical {
        left: Box<Expr<'src>>,
        op: LogicalOp,
        right: Box<Expr<'src>>,
    },
    Call {
        callee: Box<Expr<'src>>,
        args: Vec<Expr<'src>>,
    },
    Get {
        left: Box<Expr<'src>>,
        name: Identifier<'src>,
    },
    Set {
        left: Box<Expr<'src>>,
        name: Identifier<'src>,
        value: Box<Expr<'src>>,
    },
    This,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier<'src> {
    pub symbol: &'src str,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable<'src> {
    pub ident: Identifier<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Number(f64),
    String(&'src str),
    Nil,
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Minus,
    Bang,
}

bijective_enum_map::injective_enum_map! {
    UnaryOp, TokenKind<'_>,
    Minus <=> TokenKind::Minus,
    Bang <=> TokenKind::Bang,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

bijective_enum_map::injective_enum_map! {
    BinOp, TokenKind<'_>,
    Add <=> TokenKind::Plus,
    Sub <=> TokenKind::Minus,
    Mul <=> TokenKind::Star,
    Div <=> TokenKind::Slash,
    Rem <=> TokenKind::Percent,
    Equal <=> TokenKind::EqualEqual,
    NotEqual <=> TokenKind::BangEqual,
    Less <=> TokenKind::Less,
    LessEqual <=> TokenKind::LessEqual,
    Greater <=> TokenKind::Greater,
    GreaterEqual <=> TokenKind::GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicalOp {
    And,
    Or,
}

bijective_enum_map::injective_enum_map! {
    LogicalOp, TokenKind<'_>,
    And <=> TokenKind::Keyword(Keyword::And),
    Or <=> TokenKind::Keyword(Keyword::Or),
}
