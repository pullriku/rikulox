use std::{cell::RefCell, mem, rc::Rc};

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, LogicalOp, UnaryOp},
    stmt::{Stmt, StmtKind},
};
use rikulox_runtime::error::{RuntimeError, RuntimeErrorKind};

use crate::{env::Environment, value::Value};

pub struct TreeWalkInterpreter<'src> {
    env: Rc<RefCell<Environment<'src>>>,
}

impl<'src> TreeWalkInterpreter<'src> {
    pub fn new(env: Rc<RefCell<Environment<'src>>>) -> Self {
        Self { env }
    }

    pub fn interpret(
        &mut self,
        ast: Vec<Stmt<'src>>,
    ) -> Result<(), RuntimeError<'src>> {
        for stmt in ast {
            self.exec(&stmt)?
        }
        Ok(())
    }

    fn exec(&mut self, stmt: &Stmt<'src>) -> Result<(), RuntimeError<'src>> {
        match &stmt.kind {
            StmtKind::Expression(expr) => {
                self.eval(expr)?;
            }
            StmtKind::Print(_expr) => todo!(),
            StmtKind::Var {
                name: Identifier { symbol },
                init,
            } => {
                let value = match init {
                    Some(expr) => self.eval(expr)?,
                    None => Value::Nil,
                };
                self.env.borrow_mut().define(symbol, value);
            }
            StmtKind::Block(stmts) => self.exec_block(
                stmts,
                Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
                    &self.env,
                )))),
            )?,
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.eval(condition)?.is_truthy() {
                    self.exec(then_branch.as_ref())?
                } else if let Some(else_branch) = else_branch {
                    self.exec(else_branch.as_ref())?
                }
            }
            StmtKind::While { condition, body } => {
                while self.eval(condition)?.is_truthy() {
                    self.exec(body.as_ref())?
                }
            }

            StmtKind::Function(_decl) => todo!(),
        };
        Ok(())
    }

    fn exec_block(
        &mut self,
        stmts: &[Stmt<'src>],
        mut env: Rc<RefCell<Environment<'src>>>,
    ) -> Result<(), RuntimeError<'src>> {
        mem::swap(&mut self.env, &mut env);

        let result: Result<(), RuntimeError<'src>> = (|| {
            for stmt in stmts {
                self.exec(stmt)?;
            }
            Ok(())
        })();

        mem::swap(&mut self.env, &mut env);

        result
    }

    fn eval(
        &mut self,
        expr: &Expr<'src>,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        let object = match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                self.binary_expr(left.as_ref(), op, right.as_ref(), expr)?
            }
            ExprKind::Unary { op, right } => {
                let right = self.eval(right.as_ref())?;
                match op {
                    UnaryOp::Minus => match right {
                        Value::Number(n) => Value::Number(-n),
                        _ => {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::TypeError(expr.clone()),
                                span: expr.span,
                            });
                        }
                    },
                    UnaryOp::Bang => Value::Bool(right.is_truthy()),
                }
            }
            ExprKind::Grouping(expr) => self.eval(expr.as_ref())?,
            ExprKind::Literal(literal) => match literal {
                Literal::Number(number) => Value::Number(*number),
                Literal::String(_string) => todo!(),
                Literal::Nil => Value::Nil,
                Literal::Bool(bool) => Value::Bool(*bool),
            },
            ExprKind::Variable(identifier) => {
                let name = identifier.symbol;
                self.env.borrow().get(name).map_err(|kind| RuntimeError {
                    kind,
                    span: expr.span,
                })?
            }
            ExprKind::Assign { name, value } => {
                let value = self.eval(value.as_ref())?;
                let name = name.symbol;
                self.env.borrow_mut().assign(name, value.clone()).map_err(
                    |kind| RuntimeError {
                        kind,
                        span: expr.span,
                    },
                )?;
                value
            }
            ExprKind::Logical { left, op, right } => {
                let left = self.eval(left.as_ref())?;

                match op {
                    LogicalOp::And => {
                        if !left.is_truthy() {
                            return Ok(left);
                        }
                    }
                    LogicalOp::Or => {
                        if left.is_truthy() {
                            return Ok(left);
                        }
                    }
                }

                self.eval(right.as_ref())?
            }
            ExprKind::Call { callee: _, args: _ } => todo!(),
        };

        Ok(object)
    }

    fn binary_expr(
        &mut self,
        left: &Expr<'src>,
        op: &BinOp,
        right: &Expr<'src>,
        expr: &Expr<'src>,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        let (left, right) = (self.eval(left)?, self.eval(right)?);
        let object_opt = match op {
            BinOp::Add => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Number(l + r))
                }
                // TODO: Add string concatenation
                _ => None,
            },
            BinOp::Sub => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Number(l - r))
                }
                _ => None,
            },
            BinOp::Mul => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Number(l * r))
                }
                _ => None,
            },
            BinOp::Rem => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Number(l % r))
                }
                _ => None,
            },
            BinOp::Div => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Number(l / r))
                }
                _ => None,
            },
            BinOp::Greater => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Bool(l > r))
                }
                _ => None,
            },
            BinOp::GreaterEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Bool(l >= r))
                }
                _ => None,
            },
            BinOp::Less => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Bool(l < r))
                }
                _ => None,
            },
            BinOp::LessEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Some(Value::Bool(l <= r))
                }
                _ => None,
            },
            BinOp::Equal => Some(Value::Bool(left == right)),
            BinOp::NotEqual => Some(Value::Bool(left != right)),
        };

        object_opt.ok_or_else(|| RuntimeError {
            kind: RuntimeErrorKind::TypeError(expr.clone()),
            span: expr.span,
        })
    }
}
