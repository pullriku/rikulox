use std::{collections::HashMap, mem};

use rikulox_ast::{
    expr::Expr,
    id::NodeId,
    span::Span,
    stmt::{FunctionDecl, Stmt, StmtKind},
};

use crate::error::{ResolveError, ResolveErrorKind};

pub struct Resolver<'src> {
    scopes: Vec<HashMap<&'src str, bool>>,
    locals: HashMap<NodeId, usize>,
    current_function: FunctionKind,
}

impl<'src> Resolver<'src> {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            locals: HashMap::new(),
            current_function: FunctionKind::None,
        }
    }

    pub fn into_locals(self) -> HashMap<NodeId, usize> {
        self.locals
    }

    pub fn resolve(
        &mut self,
        stmts: &[Stmt<'src>],
    ) -> Result<(), ResolveError> {
        let mut errors = Vec::new();
        for stmt in stmts {
            match self.statement(stmt) {
                Ok(()) => (),
                Err(error) => errors.push(error),
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors.remove(0))
        }
    }

    fn statement(&mut self, stmt: &Stmt<'src>) -> Result<(), ResolveError> {
        let Stmt { kind, span, id: _ } = stmt;
        match kind {
            StmtKind::Expression(expr) => self.expression(expr)?,
            StmtKind::Print(expr) => self.expression(expr)?,
            StmtKind::Var { name, init } => {
                self.declare(name.symbol, *span)?;
                if let Some(init) = init {
                    self.expression(init)?;
                }
                self.define(name.symbol);
            }
            StmtKind::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts {
                    self.statement(stmt)?;
                }
                self.end_scope();
            }
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.expression(condition)?;
                self.statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.statement(else_branch)?;
                }
            }
            StmtKind::While { condition, body } => {
                self.expression(condition)?;
                self.statement(body)?;
            }
            StmtKind::Function(function_decl) => {
                self.declare(function_decl.name.symbol, *span)?;
                self.define(function_decl.name.symbol);
                self.resolve_function(
                    function_decl,
                    FunctionKind::Function,
                    *span,
                )?;
            }
            StmtKind::Return(expr) => {
                if self.current_function == FunctionKind::None {
                    return Err(ResolveError {
                        kind: ResolveErrorKind::ReturnOutsideFunction,
                        span: *span,
                    });
                }

                if let Some(expr) = expr {
                    self.expression(expr)?;
                }
            }
        }

        Ok(())
    }

    fn expression(&mut self, expr: &Expr<'src>) -> Result<(), ResolveError> {
        let Expr { kind, span, id } = expr;

        match kind {
            rikulox_ast::expr::ExprKind::Binary { left, op: _, right } => {
                self.expression(left)?;
                self.expression(right)?;
            }
            rikulox_ast::expr::ExprKind::Unary { op: _, right } => {
                self.expression(right)?
            }
            rikulox_ast::expr::ExprKind::Grouping(expr) => {
                self.expression(expr)?
            }
            rikulox_ast::expr::ExprKind::Literal(_) => (),
            rikulox_ast::expr::ExprKind::Variable(identifier) => {
                if !self.scopes.is_empty()
                    && self
                        .scopes
                        .last()
                        .unwrap()
                        .contains_key(&identifier.symbol)
                    && !self
                        .scopes
                        .last()
                        .unwrap()
                        .get(&identifier.symbol)
                        .unwrap()
                {
                    return Err(ResolveError {
                        kind: ResolveErrorKind::UninitializedVariable(
                            identifier.symbol.to_string(),
                        ),
                        span: *span,
                    });
                }
                self.resolve_local(*id, identifier.symbol);
            }
            rikulox_ast::expr::ExprKind::Assign { name, value } => {
                self.expression(value)?;
                self.resolve_local(*id, name.symbol);
            }
            rikulox_ast::expr::ExprKind::Logical { left, op: _, right } => {
                self.expression(left)?;
                self.expression(right)?;
            }
            rikulox_ast::expr::ExprKind::Call { callee, args } => {
                self.expression(callee)?;
                for arg in args {
                    self.expression(arg)?;
                }
            }
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(
        &mut self,
        name: &'src str,
        span: Span,
    ) -> Result<(), ResolveError> {
        let Some(scope) = self.scopes.last_mut() else {
            return Ok(());
        };

        if scope.contains_key(name) {
            return Err(ResolveError {
                kind: ResolveErrorKind::VariableAlreadyDeclared(
                    name.to_string(),
                ),
                span,
            });
        }

        scope.insert(name, false);

        Ok(())
    }

    fn define(&mut self, name: &'src str) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };
        scope.insert(name, true);
    }

    fn resolve_local(&mut self, id: NodeId, name: &'src str) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(name) {
                self.locals.insert(id, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        function_decl: &FunctionDecl<'src>,
        mut function_kind: FunctionKind,
        span: Span,
    ) -> Result<(), ResolveError> {
        mem::swap(&mut self.current_function, &mut function_kind);
        let enclosing_function = function_kind;

        let FunctionDecl {
            name: _,
            params,
            body,
        } = function_decl;
        self.begin_scope();
        for param in params {
            self.declare(param.symbol, span)?;
            self.define(param.symbol);
        }
        self.resolve(body)?;
        self.end_scope();

        self.current_function = enclosing_function;

        Ok(())
    }
}

impl<'src> Default for Resolver<'src> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FunctionKind {
    None,
    Function,
}
