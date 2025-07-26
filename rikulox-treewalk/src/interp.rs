use std::{cell::RefCell, mem, rc::Rc};

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, UnaryOp},
    stmt::{Stmt, StmtKind},
    string::Interner,
};
use rikulox_runtime::{
    error::{RuntimeError, RuntimeErrorKind},
    obj::Object,
};

use crate::env::Environment;

pub struct TreeWalkInterpreter {
    string_interner: Interner,
    env: Rc<RefCell<Environment>>,
}

impl TreeWalkInterpreter {
    pub fn new(string_interner: Interner, env: Rc<RefCell<Environment>>) -> Self {
        Self {
            string_interner,
            env,
        }
    }

    pub fn into_interner(self) -> Interner {
        self.string_interner
    }

    pub fn interpret(&mut self, ast: Vec<Stmt>) -> Result<(), RuntimeError> {
        for stmt in ast {
            self.exec(&stmt)?
        }
        Ok(())
    }

    fn exec(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match &stmt.kind {
            StmtKind::Expression(expr) => {
                self.eval(expr)?;
            }
            StmtKind::Print(expr) => println!("{}", self.eval(expr)?),
            StmtKind::Var {
                name: Identifier { symbol },
                init,
            } => {
                let value = match init {
                    Some(expr) => self.eval(expr)?,
                    None => Object::Nil,
                };
                self.env.borrow_mut().define(
                    self.string_interner.resolve(*symbol).unwrap().to_string(),
                    value,
                );
            }
            StmtKind::Block(stmts) => self.exec_block(
                stmts,
                Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
                    &self.env,
                )))),
            )?,
        };
        Ok(())
    }

    fn exec_block(
        &mut self,
        stmts: &[Stmt],
        mut env: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        mem::swap(&mut self.env, &mut env);

        let result: Result<(), RuntimeError> = (|| {
            for stmt in stmts {
                self.exec(stmt)?;
            }
            Ok(())
        })();

        mem::swap(&mut self.env, &mut env);

        result
    }

    fn eval(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        let object = match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                self.binary_expr(left.as_ref(), op, right.as_ref(), expr)?
            }
            ExprKind::Unary { op, right } => {
                let right = self.eval(right.as_ref())?;
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
            ExprKind::Grouping(expr) => self.eval(expr.as_ref())?,
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
            ExprKind::Variable(identifier) => {
                let name = self
                    .string_interner
                    .resolve(identifier.symbol)
                    .unwrap()
                    .to_string();
                self.env.borrow().get(&name).map_err(|kind| RuntimeError {
                    kind,
                    span: expr.span,
                })?
            }
            ExprKind::Assign { name, value } => {
                let value = self.eval(value.as_ref())?;
                let name = self
                    .string_interner
                    .resolve(name.symbol)
                    .unwrap()
                    .to_string();
                self.env
                    .borrow_mut()
                    .assign(&name, value.clone())
                    .map_err(|kind| RuntimeError {
                        kind,
                        span: expr.span,
                    })?;
                value
            }
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
        let (left, right) = (self.eval(left)?, self.eval(right)?);
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

        object_opt.ok_or_else(|| RuntimeError {
            kind: RuntimeErrorKind::TypeError(expr.clone()),
            span: expr.span,
        })
    }
}
