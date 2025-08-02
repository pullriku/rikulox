use crate::{
    call::NativeFunction, error::RuntimeError, interp::TreeWalkInterpreter,
    value::Value,
};

pub const CLOCK_FN: NativeFunction = NativeFunction {
    arity: 0,
    function: clock,
};

fn clock<'src>(
    _interp: &mut TreeWalkInterpreter<'src>,
    _args: &[Value<'src>],
) -> Result<Value<'src>, RuntimeError<'src>> {
    let now = std::time::SystemTime::now();
    Ok(Value::Number(
        now.duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    ))
}
