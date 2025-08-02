use std::{cell::RefCell, collections::HashMap, mem, rc::Rc};

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, LogicalOp, UnaryOp},
    id::NodeId,
    span::Span,
    stmt::{Stmt, StmtKind},
};

use crate::{
    call::{Class, Function},
    env::Environment,
    error::{RuntimeError, RuntimeErrorKind},
    native::CLOCK_FN,
    obj::Object,
    value::Value,
};

pub struct TreeWalkInterpreter<'src> {
    pub(crate) globals: Rc<RefCell<Environment<'src>>>,
    env: Rc<RefCell<Environment<'src>>>,
    locals: HashMap<NodeId, usize>,
}

impl<'src> TreeWalkInterpreter<'src> {
    pub fn new() -> Self {
        let env = Rc::new(RefCell::new(Environment::new()));

        env.borrow_mut().define(
            "clock",
            Value::Object(Rc::new(RefCell::new(
                Object::NativeFunction::<'src>(CLOCK_FN),
            ))),
        );

        Self {
            globals: Rc::clone(&env),
            env,
            locals: HashMap::new(),
        }
    }

    pub fn from_env(env: Rc<RefCell<Environment<'src>>>) -> Self {
        Self {
            globals: Rc::clone(&env),
            env,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(
        &mut self,
        ast: &[Stmt<'src>],
        locals: HashMap<NodeId, usize>,
    ) -> Result<(), RuntimeError<'src>> {
        self.locals.extend(locals);

        for stmt in ast {
            self.exec(stmt)?
        }
        Ok(())
    }

    fn exec(&mut self, stmt: &Stmt<'src>) -> Result<(), RuntimeError<'src>> {
        match &stmt.kind {
            StmtKind::Expression(expr) => {
                self.eval(expr)?;
            }
            StmtKind::Print(expr) => {
                println!("{}", self.eval(expr)?);
            }
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

            StmtKind::Function(declaration) => {
                let fun = Value::Object(Rc::new(RefCell::new(
                    Object::Function(Function {
                        declaration: declaration.clone(),
                        closure: Rc::clone(&self.env),
                    }),
                )));

                self.env.borrow_mut().define(declaration.name.symbol, fun);
            }
            StmtKind::Return(value) => {
                let value = value
                    .clone()
                    .map(|expr| self.eval(&expr))
                    .unwrap_or(Ok(Value::Nil))?;

                return Err(RuntimeError {
                    kind: RuntimeErrorKind::Return(value),
                    span: stmt.span,
                });
            }
            StmtKind::Class(decl) => {
                self.env.borrow_mut().define(
                    decl.name.symbol,
                    Value::Object(Rc::new(RefCell::new(Object::Class(
                        Class {
                            name: decl.name.symbol.to_string(),
                        },
                    )))),
                );
            }
        };
        Ok(())
    }

    pub(crate) fn exec_block(
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
                Literal::String(string) => Value::Object(Rc::new(
                    RefCell::new(Object::String(string.to_string())),
                )),
                Literal::Nil => Value::Nil,
                Literal::Bool(bool) => Value::Bool(*bool),
            },
            ExprKind::Variable(identifier) => {
                self.lookup_variable(identifier.symbol, expr.id, expr.span)?
            }
            ExprKind::Assign { name, value } => {
                let value = self.eval(value.as_ref())?;
                let name = name.symbol;

                let distance = self.locals.get(&expr.id);

                if let Some(distance) = distance {
                    self.env
                        .borrow_mut()
                        .assign_at(name, value.clone(), *distance)
                        .map_err(|kind| RuntimeError {
                            kind,
                            span: expr.span,
                        })?;
                } else {
                    self.globals
                        .borrow_mut()
                        .assign(name, value.clone())
                        .map_err(|kind| RuntimeError {
                            kind,
                            span: expr.span,
                        })?;
                }

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
            ExprKind::Call { callee, args } => {
                let callee = self.eval(callee.as_ref())?;
                let args = args
                    .iter()
                    .map(|arg| self.eval(arg))
                    .collect::<Result<Vec<Value<'src>>, RuntimeError<'src>>>(
                    )?;
                let Value::Object(callee) = callee else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::TypeError(expr.clone()),
                        span: expr.span,
                    });
                };

                let callee = callee.borrow();
                let callee = callee.as_call().ok_or(RuntimeError {
                    kind: RuntimeErrorKind::TypeError(expr.clone()),
                    span: expr.span,
                });

                callee?.call(self, &args, expr.span)?
            }
            ExprKind::Get { object, name } => {
                let object = self.eval(object.as_ref())?;
                
                let Value::Object(object) = object else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::TypeError(expr.clone()),
                        span: expr.span,
                    });
                };

                let Object::Instance(instance) = &*object.borrow() else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::TypeError(expr.clone()),
                        span: expr.span,
                    });
                };

                instance.get(name.symbol).ok_or(RuntimeError {
                    kind: RuntimeErrorKind::UndefinedProperty(
                        name.symbol.to_string(),
                    ),
                    span: expr.span,
                })?
            }
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
                (Value::Object(l), Value::Object(r)) => {
                    let (l, r) = (l.borrow(), r.borrow());
                    match (&*l, &*r) {
                        (Object::String(l), Object::String(r)) => {
                            Some(Value::Object(Rc::new(RefCell::new(
                                Object::String(l.to_string() + r),
                            ))))
                        }
                        _ => None,
                    }
                }
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

    fn lookup_variable(
        &self,
        name: &'src str,
        id: NodeId,
        span: Span,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        let distance = self.locals.get(&id);
        match distance {
            Some(distance) => {
                self.env
                    .borrow()
                    .get_at(name, *distance)
                    .ok_or(RuntimeError {
                        kind: RuntimeErrorKind::UndefinedVariable(
                            name.to_string(),
                        ),
                        span,
                    })
            }
            None => {
                self.globals
                    .borrow()
                    .get(name)
                    .map_err(|kind| RuntimeError {
                        kind: kind.clone(),
                        span,
                    })
            }
        }
    }
}

impl<'src> Default for TreeWalkInterpreter<'src> {
    fn default() -> Self {
        Self::new()
    }
}
