use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use rikulox_ast::stmt::FunctionDecl;

use crate::{
    env::Environment, error::RuntimeError, interp::TreeWalkInterpreter,
    obj::Object, value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Class<'src> {
    pub name: &'src str,
    pub methods: HashMap<&'src str, Function<'src>>,
}

impl<'src> Class<'src> {
    pub fn find_method(&self, name: &str) -> Option<&Function<'src>> {
        self.methods.get(name)
    }
}

impl<'src> Display for Class<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'src> {
    pub declaration: FunctionDecl<'src>,
    pub closure: Rc<RefCell<Environment<'src>>>,
    pub is_init: bool,
}

impl<'src> Function<'src> {
    pub fn bind(&mut self, instance: Value<'src>) {
        let mut env = Environment::with_enclosing(Rc::clone(&self.closure));
        env.define("this", instance);

        self.closure = Rc::new(RefCell::new(env));
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

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
