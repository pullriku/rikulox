use super::*;

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, LogicalOp, UnaryOp},
    id::{IdGen, NodeId},
    span::Span,
    stmt::{ClassDecl, FunctionDecl, Stmt, StmtKind},
};
use std::{cell::RefCell, collections::HashMap};

struct Builder {
    ids: RefCell<IdGen>,
    locals: RefCell<HashMap<NodeId, usize>>,
}

impl Builder {
    fn new() -> Self {
        Self {
            ids: RefCell::new(IdGen::new()),
            locals: RefCell::new(HashMap::new()),
        }
    }

    fn span(&self) -> Span {
        Span::empty_at(0)
    }

    fn next_id(&self) -> NodeId {
        self.ids.borrow_mut().next_id()
    }

    fn literal(&self, lit: Literal<'static>) -> Expr<'static> {
        Expr { kind: ExprKind::Literal(lit), span: self.span(), id: self.next_id() }
    }

    fn variable(&self, name: &'static str, distance: Option<usize>) -> Expr<'static> {
        let id = self.next_id();
        if let Some(d) = distance {
            self.locals.borrow_mut().insert(id, d);
        }
        Expr { kind: ExprKind::Variable(Identifier { symbol: name }), span: self.span(), id }
    }

    fn assign(&self, name: &'static str, value: Expr<'static>, distance: Option<usize>) -> Expr<'static> {
        let id = self.next_id();
        if let Some(d) = distance {
            self.locals.borrow_mut().insert(id, d);
        }
        Expr {
            kind: ExprKind::Assign { name: Identifier { symbol: name }, value: Box::new(value) },
            span: self.span(),
            id,
        }
    }

    fn unary(&self, op: UnaryOp, right: Expr<'static>) -> Expr<'static> {
        Expr {
            kind: ExprKind::Unary { op, right: Box::new(right) },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn binary(&self, left: Expr<'static>, op: BinOp, right: Expr<'static>) -> Expr<'static> {
        Expr {
            kind: ExprKind::Binary { left: Box::new(left), op, right: Box::new(right) },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn logical(&self, left: Expr<'static>, op: LogicalOp, right: Expr<'static>) -> Expr<'static> {
        Expr {
            kind: ExprKind::Logical { left: Box::new(left), op, right: Box::new(right) },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn call(&self, callee: Expr<'static>, args: Vec<Expr<'static>>) -> Expr<'static> {
        Expr {
            kind: ExprKind::Call { callee: Box::new(callee), args },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn get(&self, left: Expr<'static>, name: &'static str) -> Expr<'static> {
        Expr {
            kind: ExprKind::Get { left: Box::new(left), name: Identifier { symbol: name } },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn set(&self, left: Expr<'static>, name: &'static str, value: Expr<'static>) -> Expr<'static> {
        Expr {
            kind: ExprKind::Set { left: Box::new(left), name: Identifier { symbol: name }, value: Box::new(value) },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn this(&self, distance: usize) -> Expr<'static> {
        let id = self.next_id();
        self.locals.borrow_mut().insert(id, distance);
        Expr { kind: ExprKind::This, span: self.span(), id }
    }

    fn expr_stmt(&self, expr: Expr<'static>) -> Stmt<'static> {
        Stmt { kind: StmtKind::Expression(expr), span: self.span(), id: self.next_id() }
    }

    fn var_stmt(&self, name: &'static str, init: Option<Expr<'static>>) -> Stmt<'static> {
        Stmt {
            kind: StmtKind::Var { name: Identifier { symbol: name }, init },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn block_stmt(&self, stmts: Vec<Stmt<'static>>) -> Stmt<'static> {
        Stmt { kind: StmtKind::Block(stmts), span: self.span(), id: self.next_id() }
    }

    fn if_stmt(&self, cond: Expr<'static>, then_branch: Stmt<'static>, else_branch: Option<Stmt<'static>>) -> Stmt<'static> {
        Stmt {
            kind: StmtKind::If { condition: cond, then_branch: Box::new(then_branch), else_branch: else_branch.map(Box::new) },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn while_stmt(&self, cond: Expr<'static>, body: Stmt<'static>) -> Stmt<'static> {
        Stmt {
            kind: StmtKind::While { condition: cond, body: Box::new(body) },
            span: self.span(),
            id: self.next_id(),
        }
    }

    fn return_stmt(&self, value: Option<Expr<'static>>) -> Stmt<'static> {
        Stmt { kind: StmtKind::Return(value), span: self.span(), id: self.next_id() }
    }

    fn function_decl(&self, name: &'static str, params: Vec<&'static str>, body: Vec<Stmt<'static>>) -> FunctionDecl<'static> {
        FunctionDecl {
            name: Identifier { symbol: name },
            params: params.into_iter().map(|p| Identifier { symbol: p }).collect(),
            body,
        }
    }

    fn function_stmt(&self, name: &'static str, params: Vec<&'static str>, body: Vec<Stmt<'static>>) -> Stmt<'static> {
        let decl = self.function_decl(name, params, body);
        Stmt { kind: StmtKind::Function(decl), span: self.span(), id: self.next_id() }
    }

    fn class_stmt(&self, name: &'static str, methods: Vec<FunctionDecl<'static>>) -> Stmt<'static> {
        Stmt {
            kind: StmtKind::Class(ClassDecl { name: Identifier { symbol: name }, methods }),
            span: self.span(),
            id: self.next_id(),
        }
    }
}

fn run<F>(build: F) -> Result<TreeWalkInterpreter<'static>, RuntimeError<'static>>
where
    F: FnOnce(&Builder) -> Vec<Stmt<'static>>,
{
    let builder = Builder::new();
    let stmts = build(&builder);
    let locals = builder.locals.into_inner();
    let mut interp = TreeWalkInterpreter::new();
    match interp.interpret(&stmts, locals) {
        Ok(()) => Ok(interp),
        Err(e) => Err(e),
    }
}

#[test]
fn variable_definition_and_get() {
    let interp = run(|b| {
        vec![b.var_stmt("a", Some(b.literal(Literal::Number(10.0))))]
    }).unwrap();
    assert_eq!(interp.globals.borrow().get("a").unwrap(), Value::Number(10.0));
}

#[test]
fn uninitialized_variable_is_nil() {
    let interp = run(|b| vec![b.var_stmt("a", None)]).unwrap();
    assert_eq!(interp.globals.borrow().get("a").unwrap(), Value::Nil);
}

#[test]
fn block_scope_does_not_affect_outer() {
    let interp = run(|b| {
        vec![
            b.var_stmt("a", Some(b.literal(Literal::Number(10.0)))),
            b.block_stmt(vec![b.var_stmt("a", Some(b.literal(Literal::Number(20.0))))]),
        ]
    }).unwrap();
    assert_eq!(interp.globals.borrow().get("a").unwrap(), Value::Number(10.0));
}

#[test]
fn reassignment_updates_value() {
    let interp = run(|b| {
        vec![
            b.var_stmt("a", Some(b.literal(Literal::Number(10.0)))),
            b.expr_stmt(b.assign("a", b.literal(Literal::Number(20.0)), None)),
        ]
    }).unwrap();
    assert_eq!(interp.globals.borrow().get("a").unwrap(), Value::Number(20.0));
}

#[test]
fn binary_and_logical_operations() {
    let interp = run(|b| {
        vec![
            b.var_stmt("add", Some(b.binary(
                b.literal(Literal::Number(1.0)),
                BinOp::Add,
                b.literal(Literal::Number(2.0)),
            ))),
            b.var_stmt("sub", Some(b.binary(
                b.literal(Literal::Number(5.0)),
                BinOp::Sub,
                b.literal(Literal::Number(3.0)),
            ))),
            b.var_stmt("mul", Some(b.binary(
                b.literal(Literal::Number(4.0)),
                BinOp::Mul,
                b.literal(Literal::Number(2.0)),
            ))),
            b.var_stmt("div", Some(b.binary(
                b.literal(Literal::Number(8.0)),
                BinOp::Div,
                b.literal(Literal::Number(2.0)),
            ))),
            b.var_stmt("cmp", Some(b.binary(
                b.literal(Literal::Number(5.0)),
                BinOp::Greater,
                b.literal(Literal::Number(3.0)),
            ))),
            b.var_stmt("eq", Some(b.binary(
                b.literal(Literal::Number(3.0)),
                BinOp::Equal,
                b.literal(Literal::Number(3.0)),
            ))),
            b.var_stmt("concat", Some(b.binary(
                b.literal(Literal::String("a")),
                BinOp::Add,
                b.literal(Literal::String("b")),
            ))),
            b.var_stmt("neg", Some(b.unary(
                UnaryOp::Minus,
                b.literal(Literal::Number(1.0)),
            ))),
            b.var_stmt("not", Some(b.unary(
                UnaryOp::Bang,
                b.literal(Literal::Bool(true)),
            ))),
            b.var_stmt("lor", Some(b.logical(
                b.literal(Literal::Bool(true)),
                LogicalOp::Or,
                b.literal(Literal::Bool(false)),
            ))),
            b.var_stmt("land", Some(b.logical(
                b.literal(Literal::Bool(false)),
                LogicalOp::And,
                b.literal(Literal::Bool(true)),
            ))),
        ]
    }).unwrap();
    let g = interp.globals.borrow();
    assert_eq!(g.get("add").unwrap(), Value::Number(3.0));
    assert_eq!(g.get("sub").unwrap(), Value::Number(2.0));
    assert_eq!(g.get("mul").unwrap(), Value::Number(8.0));
    assert_eq!(g.get("div").unwrap(), Value::Number(4.0));
    assert_eq!(g.get("cmp").unwrap(), Value::Bool(true));
    assert_eq!(g.get("eq").unwrap(), Value::Bool(true));
    if let Value::Object(obj) = g.get("concat").unwrap() {
        assert!(matches!(&*obj.borrow(), Object::String(s) if s == "ab"));
    } else {
        panic!("concat is not a string");
    }
    assert_eq!(g.get("neg").unwrap(), Value::Number(-1.0));
    assert_eq!(g.get("not").unwrap(), Value::Bool(false));
    assert_eq!(g.get("lor").unwrap(), Value::Bool(true));
    assert_eq!(g.get("land").unwrap(), Value::Bool(false));
}

#[test]
fn mixing_number_and_string_errors() {
    let err = match run(|b| {
        vec![b.var_stmt("x", Some(b.binary(
            b.literal(Literal::String("a")),
            BinOp::Add,
            b.literal(Literal::Number(1.0)),
        )))]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::TypeError(_)));
}

#[test]
fn if_else_and_while() {
    let interp = run(|b| {
        let assign_a1 = b.expr_stmt(b.assign("a", b.literal(Literal::Number(1.0)), None));
        let assign_a2 = b.expr_stmt(b.assign("a", b.literal(Literal::Number(2.0)), None));
        let assign_b1 = b.expr_stmt(b.assign("b", b.literal(Literal::Number(1.0)), None));
        let assign_b2 = b.expr_stmt(b.assign("b", b.literal(Literal::Number(2.0)), None));
        let while_body = b.expr_stmt(b.assign(
            "i",
            b.binary(b.variable("i", None), BinOp::Add, b.literal(Literal::Number(1.0))),
            None,
        ));
        vec![
            b.var_stmt("a", Some(b.literal(Literal::Number(0.0)))),
            b.if_stmt(b.literal(Literal::Bool(true)), assign_a1, Some(assign_a2)),
            b.var_stmt("b", Some(b.literal(Literal::Number(0.0)))),
            b.if_stmt(b.literal(Literal::Bool(false)), assign_b1, Some(assign_b2)),
            b.var_stmt("i", Some(b.literal(Literal::Number(0.0)))),
            b.while_stmt(
                b.binary(b.variable("i", None), BinOp::Less, b.literal(Literal::Number(3.0))),
                while_body,
            ),
        ]
    }).unwrap();
    let g = interp.globals.borrow();
    assert_eq!(g.get("a").unwrap(), Value::Number(1.0));
    assert_eq!(g.get("b").unwrap(), Value::Number(2.0));
    assert_eq!(g.get("i").unwrap(), Value::Number(3.0));
}

#[test]
fn block_variable_does_not_leak() {
    let err = match run(|b| {
        vec![
            b.block_stmt(vec![b.var_stmt("a", Some(b.literal(Literal::Number(1.0))))]),
            b.expr_stmt(b.variable("a", None)),
        ]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::UndefinedVariable(_)));
}

#[test]
fn function_definition_and_call() {
    let interp = run(|b| {
        let body = vec![b.return_stmt(Some(b.binary(
            b.variable("a", Some(0)),
            BinOp::Add,
            b.variable("b", Some(0)),
        )))];
        vec![
            b.function_stmt("add", vec!["a", "b"], body),
            b.var_stmt(
                "result",
                Some(b.call(
                    b.variable("add", None),
                    vec![b.literal(Literal::Number(2.0)), b.literal(Literal::Number(3.0))],
                )),
            ),
        ]
    }).unwrap();
    assert_eq!(interp.globals.borrow().get("result").unwrap(), Value::Number(5.0));
}

#[test]
fn recursive_function_factorial() {
    let interp = run(|b| {
        let cond = b.binary(
            b.variable("n", Some(0)),
            BinOp::LessEqual,
            b.literal(Literal::Number(1.0)),
        );
        let then_branch = b.return_stmt(Some(b.literal(Literal::Number(1.0))));
        let if_stmt = b.if_stmt(cond, then_branch, None);
        let call = b.call(
            b.variable("fact", None),
            vec![b.binary(
                b.variable("n", Some(0)),
                BinOp::Sub,
                b.literal(Literal::Number(1.0)),
            )],
        );
        let ret = b.return_stmt(Some(b.binary(
            b.variable("n", Some(0)),
            BinOp::Mul,
            call,
        )));
        vec![
            b.function_stmt("fact", vec!["n"], vec![if_stmt, ret]),
            b.var_stmt(
                "result",
                Some(b.call(b.variable("fact", None), vec![b.literal(Literal::Number(5.0))])),
            ),
        ]
    }).unwrap();
    assert_eq!(interp.globals.borrow().get("result").unwrap(), Value::Number(120.0));
}

#[test]
fn arity_error_too_few() {
    let err = match run(|b| {
        vec![
            b.function_stmt("foo", vec!["a", "b"], vec![]),
            b.expr_stmt(b.call(b.variable("foo", None), vec![b.literal(Literal::Number(1.0))])),
        ]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::Arity { expected: 2, actual: 1 }));
}

#[test]
fn arity_error_too_many() {
    let err = match run(|b| {
        vec![
            b.function_stmt("foo", vec!["a"], vec![]),
            b.expr_stmt(b.call(
                b.variable("foo", None),
                vec![b.literal(Literal::Number(1.0)), b.literal(Literal::Number(2.0))],
            )),
        ]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::Arity { expected: 1, actual: 2 }));
}

#[test]
fn class_instantiation_and_property() {
    let interp = run(|b| {
        vec![
            b.class_stmt("Foo", vec![]),
            b.var_stmt("f", Some(b.call(b.variable("Foo", None), vec![]))),
            b.expr_stmt(b.set(
                b.variable("f", None),
                "field",
                b.literal(Literal::Number(10.0)),
            )),
            b.var_stmt("result", Some(b.get(b.variable("f", None), "field"))),
        ]
    }).unwrap();
    let g = interp.globals.borrow();
    if let Value::Object(obj) = g.get("f").unwrap() {
        assert!(matches!(&*obj.borrow(), Object::Instance(_)));
    } else {
        panic!("f is not an object");
    }
    assert_eq!(g.get("result").unwrap(), Value::Number(10.0));
}

#[test]
fn method_call_with_this_and_init() {
    let interp = run(|b| {
        let init_body = vec![b.expr_stmt(b.set(
            b.this(1),
            "value",
            b.literal(Literal::Number(1.0)),
        ))];
        let get_body = vec![b.return_stmt(Some(b.get(b.this(1), "value")))];
        let init_decl = b.function_decl("init", vec![], init_body);
        let get_decl = b.function_decl("get", vec![], get_body);
        vec![
            b.class_stmt("Foo", vec![init_decl, get_decl]),
            b.var_stmt("f", Some(b.call(b.variable("Foo", None), vec![]))),
            b.var_stmt(
                "result",
                Some(b.call(b.get(b.variable("f", None), "get"), vec![])),
            ),
        ]
    }).unwrap();
    assert_eq!(interp.globals.borrow().get("result").unwrap(), Value::Number(1.0));
}

#[test]
fn return_value_and_nil() {
    let interp = run(|b| {
        vec![
            b.function_stmt("foo", vec![], vec![b.return_stmt(Some(b.literal(Literal::Number(42.0))))]),
            b.function_stmt("bar", vec![], vec![b.return_stmt(None)]),
            b.var_stmt("a", Some(b.call(b.variable("foo", None), vec![]))),
            b.var_stmt("b", Some(b.call(b.variable("bar", None), vec![]))),
        ]
    }).unwrap();
    let g = interp.globals.borrow();
    assert_eq!(g.get("a").unwrap(), Value::Number(42.0));
    assert_eq!(g.get("b").unwrap(), Value::Nil);
}

#[test]
fn undefined_variable_error() {
    let err = match run(|b| vec![b.expr_stmt(b.variable("a", None))]) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::UndefinedVariable(_)));
}

#[test]
fn undefined_property_error() {
    let err = match run(|b| {
        vec![
            b.class_stmt("Foo", vec![]),
            b.var_stmt("f", Some(b.call(b.variable("Foo", None), vec![]))),
            b.expr_stmt(b.get(b.variable("f", None), "bar")),
        ]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::UndefinedProperty(_)));
}

#[test]
fn type_error_function_object() {
    let err = match run(|b| {
        vec![
            b.function_stmt("foo", vec![], vec![]),
            b.var_stmt(
                "x",
                Some(b.binary(
                    b.variable("foo", None),
                    BinOp::Add,
                    b.literal(Literal::Number(1.0)),
                )),
            ),
        ]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::TypeError(_)));
}

#[test]
fn calling_non_function_is_error() {
    let err = match run(|b| {
        vec![
            b.var_stmt("a", Some(b.literal(Literal::Number(1.0)))),
            b.expr_stmt(b.call(b.variable("a", None), vec![])),
        ]
    }) {
        Ok(_) => panic!("expected error"),
        Err(e) => e,
    };
    assert!(matches!(err.kind, RuntimeErrorKind::TypeError(_)));
}

#[test]
fn clock_returns_number() {
    let interp = run(|b| {
        vec![b.var_stmt("t", Some(b.call(b.variable("clock", None), vec![])))]
    }).unwrap();
    assert!(matches!(interp.globals.borrow().get("t").unwrap(), Value::Number(_)));
}
