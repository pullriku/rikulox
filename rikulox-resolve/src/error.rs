use rikulox_ast::span::Span;

#[derive(Debug, Clone)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ResolveErrorKind {
    UninitializedVariable(String),
    VariableAlreadyDeclared(String),
    ReturnOutsideFunction,
}
