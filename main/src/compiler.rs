use crate::context::{Arena, GlobalContext};

pub fn run_compiler(input: String) {
    let arena = Arena::new();
    let global_ctxt = GlobalContext::new(&arena);

    let parsed = parser::lex_and_parse(&arena, &input).unwrap();
    let _typed = typing::typeck(parsed, global_ctxt.typing_context()).unwrap();
}
