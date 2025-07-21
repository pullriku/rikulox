use rikulox_ast::ast::Ast;

use crate::error::RuntimeError;

pub trait Interpreter {
    fn interpret(&mut self, ast: Ast) -> Result<(), RuntimeError>;
}
