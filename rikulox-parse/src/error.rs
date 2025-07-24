use rikulox_ast::{span::Span, token::TokenKind};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken(TokenKind),
    UnexpectedEof,
    RParenNotFound,
}
