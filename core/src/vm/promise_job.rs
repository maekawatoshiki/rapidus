use super::jsvalue::value::Value;

pub enum PromiseJob {
    Reaction {
        state: i32,
        result: Value,
        child: Value,
        on_fulfilled: Value,
        on_rejected: Value,
    },
    ResolveThenable {
        promise: Value,
        thenable: Value,
        then_action: Value,
    },
}
