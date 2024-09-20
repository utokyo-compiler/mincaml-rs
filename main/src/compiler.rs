use middleware::{Arena, GlobalContext};

pub fn run_compiler(input: String) {
    let arena = Arena::default();
    let global_ctxt = GlobalContext::new(&arena);

    let parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), &input).unwrap();
    let typed_tree = typing::typeck(
        global_ctxt.typing_context(),
        &global_ctxt.common_types,
        parsed_tree,
    )
    .unwrap();
    let knorm_tree = ir_knorm::lowering(global_ctxt.knorm_context(), typed_tree);
    let _closure_tree = ir_closure::lowering(global_ctxt.closure_context(), knorm_tree);
}
