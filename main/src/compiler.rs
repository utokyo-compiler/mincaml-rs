use middleware::{Arena, GlobalContext};

pub fn run_compiler(input: String) {
    let arena = Arena::default();
    let global_ctxt = GlobalContext::new(&arena);

    let parsed = parser::lex_and_parse(global_ctxt.parsing_context(), &input).unwrap();
    let _typed = typing::typeck(
        global_ctxt.typing_context(),
        &global_ctxt.common_types,
        parsed,
    )
    .unwrap();
}
