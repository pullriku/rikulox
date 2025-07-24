use rikulox_ast::span::Span;

#[derive(Debug, Clone)]
pub struct ScanError {
    pub kind: ScanErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ScanErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
}
