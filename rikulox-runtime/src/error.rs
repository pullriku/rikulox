use rikulox_ast::{expr::Expr, span::Span};

#[derive(Debug, Clone)]
pub struct RuntimeError<'src> {
    pub kind: RuntimeErrorKind<'src>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind<'src> {
    TypeError(Expr<'src>),
    UndefinedVariable(String),
    Arity { expected: usize, actual: usize },
}
