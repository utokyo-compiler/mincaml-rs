use middleware::{Arena, GlobalContext, Option};

pub fn run(input: &str, compiler_option: Option) {
    let arena = Arena::default();
    let global_ctxt = GlobalContext::new(&arena, compiler_option);

    let parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), input).unwrap();
    let typed_tree = typing::typeck(
        global_ctxt.typing_context(),
        &global_ctxt.common_types,
        parsed_tree,
    )
    .unwrap();
    let knorm_tree = ir_knorm::lowering(global_ctxt.knorm_context(), typed_tree);
    let closure_prog = ir_closure::lowering(global_ctxt.closure_context(), knorm_tree);
    let _asm_virtual_prog =
        ir_asm_virtual::lowering(global_ctxt.asm_virtual_context(), closure_prog);
}
