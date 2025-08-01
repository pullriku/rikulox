use std::{cell::RefCell, mem, rc::Rc};

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, LogicalOp, UnaryOp},
    stmt::{Stmt, StmtKind},
    string::Interner,
};
use rikulox_runtime::error::{RuntimeError, RuntimeErrorKind};

use crate::{env::Environment, value::Value};

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
                    None => Value::Nil,
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

    fn eval(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
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
                Literal::String(symbol_u32) => Value::String(
                    self.string_interner
                        .resolve(*symbol_u32)
                        .unwrap()
                        .to_string(),
                ),
                Literal::Nil => Value::Nil,
                Literal::Bool(bool) => Value::Bool(*bool),
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
        };

        Ok(object)
    }

    fn binary_expr(
        &mut self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
        expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        let (left, right) = (self.eval(left)?, self.eval(right)?);
        let object_opt = match op {
            BinOp::Add => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Some(Value::String(l + &r)),
                _ => None,
            },
            BinOp::Sub => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Number(l - r)),
                _ => None,
            },
            BinOp::Mul => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Number(l * r)),
                _ => None,
            },
            BinOp::Rem => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Number(l % r)),
                _ => None,
            },
            BinOp::Div => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Number(l / r)),
                _ => None,
            },
            BinOp::Greater => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Bool(l > r)),
                _ => None,
            },
            BinOp::GreaterEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Bool(l >= r)),
                _ => None,
            },
            BinOp::Less => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Bool(l < r)),
                _ => None,
            },
            BinOp::LessEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Some(Value::Bool(l <= r)),
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
