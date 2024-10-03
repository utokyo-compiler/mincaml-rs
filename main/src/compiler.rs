use middleware::{session::Session, Arena, GlobalContext};

pub fn run(session: Session) {
    let arena = Arena::default();
    let global_ctxt = GlobalContext::new(&arena, session);

    let input = global_ctxt.session().input.concatenated_string();
    let parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), &input).unwrap();
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
