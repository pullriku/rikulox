use rikulox_ast::{span::Span, token::TokenKind};

#[derive(Debug, Clone)]
pub struct ParseError<'src> {
    pub kind: ParseErrorKind<'src>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind<'src> {
    UnexpectedToken {
        expected: ExpectedItem<'src>,
        found: TokenKind<'src>,
    },
    UnexpectedEof {
        expected: ExpectedItem<'src>,
    },
    RParenNotFound,
    TooManyArguments,
}

#[derive(Debug, Clone, Copy)]
pub enum ExpectedItem<'src> {
    Stmt,
    Expr,
    Ident,
    Token(TokenKind<'src>),
    Unknown,
}
