use rikulox_ast::{
    ast::{BinOp, Expr, ExprKind, Literal, UnaryOp},
    string::Interner,
};
use rikulox_runtime::{
    error::{RuntimeError, RuntimeErrorKind},
    obj::Object,
};

pub struct TreeWalkInterpreter {
    string_interner: Interner,
}

impl TreeWalkInterpreter {
    pub fn new(string_interner: Interner) -> Self {
        Self { string_interner }
    }

    pub fn interpret(&mut self, ast: Expr) -> Result<(), RuntimeError> {
        let object = self.expression(&ast)?;
        println!("{object}");
        Ok(())
    }

    fn expression(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        let object = match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                self.binary_expr(left.as_ref(), op, right.as_ref(), expr)?
            }
            ExprKind::Unary { op, right } => {
                let right = self.expression(right.as_ref())?;
                match op {
                    UnaryOp::Minus => match right {
                        Object::Number(n) => Object::Number(-n),
                        _ => {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::TypeError(expr.clone()),
                                span: expr.span,
                            });
                        }
                    },
                    UnaryOp::Bang => Object::Bool(right.is_truthy()),
                }
            }
            ExprKind::Grouping(expr) => self.expression(expr.as_ref())?,
            ExprKind::Literal(literal) => match literal {
                Literal::Number(number) => Object::Number(*number),
                Literal::String(symbol_u32) => Object::String(
                    self.string_interner
                        .resolve(*symbol_u32)
                        .unwrap()
                        .to_string(),
                ),
                Literal::Nil => Object::Nil,
                Literal::Bool(bool) => Object::Bool(*bool),
            },
        };

        Ok(object)
    }

    fn binary_expr(
        &mut self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
        expr: &Expr,
    ) -> Result<Object, RuntimeError> {
        let (left, right) = (self.expression(left)?, self.expression(right)?);
        let object_opt = match op {
            BinOp::Add => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Number(l + r)),
                (Object::String(l), Object::String(r)) => Some(Object::String(l + &r)),
                _ => None,
            },
            BinOp::Sub => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Number(l - r)),
                _ => None,
            },
            BinOp::Mul => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Number(l * r)),
                _ => None,
            },
            BinOp::Rem => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Number(l % r)),
                _ => None,
            },
            BinOp::Div => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Number(l / r)),
                _ => None,
            },
            BinOp::Greater => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Bool(l > r)),
                _ => None,
            },
            BinOp::GreaterEqual => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Bool(l >= r)),
                _ => None,
            },
            BinOp::Less => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Bool(l < r)),
                _ => None,
            },
            BinOp::LessEqual => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Some(Object::Bool(l <= r)),
                _ => None,
            },
            BinOp::Equal => Some(Object::Bool(left == right)),
            BinOp::NotEqual => Some(Object::Bool(left != right)),
        };

        object_opt.ok_or(RuntimeError {
            kind: RuntimeErrorKind::TypeError(expr.clone()),
            span: expr.span,
        })
    }
}
