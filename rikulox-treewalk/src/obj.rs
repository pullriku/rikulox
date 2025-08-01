use std::fmt::Display;

use rikulox_ast::stmt::{FunctionDecl, Stmt, StmtKind};
use rikulox_gc::trace::Trace;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Function {
        declaration: FunctionDecl,
    }
}

impl Trace for Object {
    fn trace(&self, tracer: &mut rikulox_gc::trace::Tracer<Self>)
        where
            Self: Sized {
        todo!()
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Function(FunctionDecl { name, params, body }) => {
                write!(f, "fn {}({})", name, params.join(", "))
            }
        }
    }
}
