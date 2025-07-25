use rikulox_ast::{expr::Expr, span::Span};

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    TypeError(Expr),
    UndefinedVariable(String),
}
