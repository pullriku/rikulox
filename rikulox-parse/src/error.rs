use rikulox_ast::{span::Span, token::TokenKind};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken {
        expected: ExpectedItem,
        found: TokenKind,
    },
    UnexpectedEof {
        expected: ExpectedItem,
    },
    RParenNotFound,
    TooManyArguments,
}

#[derive(Debug, Clone, Copy)]
pub enum ExpectedItem {
    Stmt,
    Expr,
    Ident,
    Token(TokenKind),
    Unknown,
}
