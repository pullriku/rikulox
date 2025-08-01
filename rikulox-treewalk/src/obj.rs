use std::fmt::Display;

use rikulox_ast::stmt::FunctionDecl;
use rikulox_gc::trace::Trace;

#[derive(Debug, Clone, PartialEq)]
pub enum Object<'src> {
    String(String),
    Function { declaration: FunctionDecl<'src> },
}

impl<'src> Trace for Object<'src> {
    fn trace(&self, tracer: &mut rikulox_gc::trace::Tracer<Self>)
    where
        Self: Sized,
    {
        todo!("{:?}", tracer)
    }
}

impl<'src> Display for Object<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{s}"),
            Object::Function {
                declaration: FunctionDecl { name, params, .. },
            } => {
                write!(
                    f,
                    "fn {}({})",
                    name.symbol,
                    params
                        .iter()
                        .map(|p| p.symbol)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}
