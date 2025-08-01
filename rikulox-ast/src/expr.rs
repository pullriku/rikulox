use crate::{
    id::NodeId,
    span::Span,
    string::InternSymbol,
    token::{Keyword, TokenKind},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(Literal),
    Variable(Identifier),
    Assign {
        name: Identifier,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: LogicalOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub symbol: InternSymbol,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(InternSymbol),
    Nil,
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Minus,
    Bang,
}

bijective_enum_map::injective_enum_map! {
    UnaryOp, TokenKind,
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
    BinOp, TokenKind,
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
    LogicalOp, TokenKind,
    And <=> TokenKind::Keyword(Keyword::And),
    Or <=> TokenKind::Keyword(Keyword::Or),
}
