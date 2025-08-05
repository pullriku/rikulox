use std::{collections::HashMap, mem};

use rikulox_ast::{
    expr::{Expr, ExprKind, Variable},
    id::NodeId,
    span::Span,
    stmt::{FunctionDecl, Stmt, StmtKind},
};

use crate::error::{ResolveError, ResolveErrorKind};

pub struct Resolver<'src> {
    scopes: Vec<HashMap<&'src str, bool>>,
    locals: HashMap<NodeId, usize>,
    current_function: FunctionKind,
    current_class: ClassKind,
}

impl<'src> Resolver<'src> {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            locals: HashMap::new(),
            current_function: FunctionKind::None,
            current_class: ClassKind::None,
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
        let Stmt { kind, span, id } = stmt;
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
                    if self.current_function == FunctionKind::Init {
                        return Err(ResolveError {
                            kind: ResolveErrorKind::ReturnInInit,
                            span: *span,
                        });
                    }

                    self.expression(expr)?;
                }
            }

            StmtKind::Class(class_decl) => {
                let enclosing_class =
                    mem::replace(&mut self.current_class, ClassKind::Class);

                self.declare(class_decl.name.symbol, *span)?;
                self.define(class_decl.name.symbol);

                if let Some(superclass_var) = &class_decl.superclass {
                    if class_decl.name.symbol == superclass_var.ident.symbol {
                        return Err(ResolveError {
                            kind: ResolveErrorKind::ClassCannotExtendItself,
                            span: *span,
                        });
                    }

                    self.resolve_variable(superclass_var, *span, *id)?;
                }

                self.begin_scope();

                self.scopes.last_mut().unwrap().insert("this", true);

                for method in &class_decl.methods {
                    let func_kind = if method.name.symbol == "init" {
                        FunctionKind::Init
                    } else {
                        FunctionKind::Method
                    };
                    self.resolve_function(method, func_kind, *span)?;
                }

                self.end_scope();

                self.current_class = enclosing_class;
            }
        }

        Ok(())
    }

    fn expression(&mut self, expr: &Expr<'src>) -> Result<(), ResolveError> {
        let Expr { kind, span, id } = expr;

        match kind {
            ExprKind::Binary { left, op: _, right } => {
                self.expression(left)?;
                self.expression(right)?;
            }
            ExprKind::Unary { op: _, right } => self.expression(right)?,
            ExprKind::Grouping(expr) => self.expression(expr)?,
            ExprKind::Literal(_) => (),
            ExprKind::Variable(var) => {
                self.resolve_variable(var, *span, *id)?;
            }
            ExprKind::Assign { name, value } => {
                self.expression(value)?;
                self.resolve_local(*id, name.symbol);
            }
            ExprKind::Logical { left, op: _, right } => {
                self.expression(left)?;
                self.expression(right)?;
            }
            ExprKind::Call { callee, args } => {
                self.expression(callee)?;
                for arg in args {
                    self.expression(arg)?;
                }
            }
            ExprKind::Get {
                left: object,
                name: _,
            } => {
                self.expression(object)?;
            }
            ExprKind::Set {
                left: object,
                name: _,
                value,
            } => {
                self.expression(object)?;
                self.expression(value)?;
            }
            ExprKind::This => {
                if self.current_class == ClassKind::None {
                    return Err(ResolveError {
                        kind: ResolveErrorKind::ThisOutsideClass,
                        span: *span,
                    });
                }
                self.resolve_local(*id, "this");
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

    fn resolve_variable(
        &mut self,
        var: &Variable<'src>,
        span: Span,
        id: NodeId,
    ) -> Result<(), ResolveError> {
        let identifier = &var.ident;
        if !self.scopes.is_empty()
            && self.scopes.last().unwrap().contains_key(&identifier.symbol)
            && !self.scopes.last().unwrap().get(&identifier.symbol).unwrap()
        {
            return Err(ResolveError {
                kind: ResolveErrorKind::UninitializedVariable(
                    identifier.symbol.to_string(),
                ),
                span,
            });
        }
        self.resolve_local(id, identifier.symbol);

        Ok(())
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
    Init,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ClassKind {
    None,
    Class,
}
