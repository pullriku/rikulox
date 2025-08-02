use std::{cell::RefCell, fmt::Display, rc::Rc};

use rikulox_ast::{span::Span, stmt::FunctionDecl};

use crate::{
    env::Environment,
    error::{RuntimeError, RuntimeErrorKind},
    interp::TreeWalkInterpreter,
    obj::{Instance, Object},
    value::Value,
};

pub trait Call<'src> {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interp: &mut TreeWalkInterpreter<'src>,
        args: &[Value<'src>],
        call_span: Span,
    ) -> Result<Value<'src>, RuntimeError<'src>>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
}

impl<'src> Call<'src> for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(
            &self,
            interp: &mut TreeWalkInterpreter<'src>,
            args: &[Value<'src>],
            call_span: Span,
        ) -> Result<Value<'src>, RuntimeError<'src>> {
        
        Ok(Value::Object(Rc::new(RefCell::new(Object::Instance(Instance {
            class: self.clone(),
        })))))
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'src> {
    pub declaration: FunctionDecl<'src>,
    pub closure: Rc<RefCell<Environment<'src>>>,
}

impl<'src> Call<'src> for Function<'src> {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &self,
        interp: &mut TreeWalkInterpreter<'src>,
        args: &[Value<'src>],
        call_span: Span,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        if self.arity() != args.len() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::Arity {
                    expected: self.arity(),
                    actual: args.len(),
                },
                span: call_span,
            });
        }

        let mut env = Environment::with_enclosing(Rc::clone(&self.closure));

        for (param, arg) in self.declaration.params.iter().zip(args) {
            env.define(param.symbol, arg.clone());
        }

        let result = interp
            .exec_block(&self.declaration.body, Rc::new(RefCell::new(env)));

        if let Err(RuntimeError {
            kind: RuntimeErrorKind::Return(value),
            span: _,
        }) = result
        {
            Ok(value)
        } else {
            result?;
            Ok(Value::Nil)
        }
    }
}

impl<'src> Display for Function<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn {}({})",
            self.declaration.name.symbol,
            self.declaration
                .params
                .iter()
                .map(|p| p.symbol)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub arity: usize,
    pub function: for<'src> fn(
        &mut TreeWalkInterpreter<'src>,
        &[Value<'src>],
    ) -> Result<Value<'src>, RuntimeError<'src>>,
}

impl NativeFunction {
    pub fn into_shared_object<'src>(self) -> Rc<RefCell<Object<'src>>> {
        Rc::new(RefCell::new(Object::NativeFunction(self)))
    }
}

impl<'src> Call<'src> for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(
        &self,
        interp: &mut TreeWalkInterpreter<'src>,
        args: &[Value<'src>],
        call_span: Span,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        if self.arity() != args.len() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::Arity {
                    expected: self.arity,
                    actual: args.len(),
                },
                span: call_span,
            });
        }
        (self.function)(interp, args)
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
