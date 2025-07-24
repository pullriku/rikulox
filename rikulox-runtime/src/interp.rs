use rikulox_ast::ast::Expr;

use crate::error::RuntimeError;

pub trait Interpreter {
    fn interpret(&mut self, ast: Expr) -> Result<(), RuntimeError>;
}
