use std::io::Write;

use middleware::{Arena, GlobalContext};
use session::Session;

pub fn run(session: Session) {
    let arena = Arena::default();
    let global_ctxt = GlobalContext::new(&arena, session);

    let input_interface = global_ctxt.session().input_interface.concatenated_string();
    let parsed_interface =
        parser::lex_and_parse_mli(global_ctxt.parsing_context(), &input_interface).unwrap();
    let input = global_ctxt.session().input.concatenated_string();
    let parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), &input).unwrap();
    let typed_tree = typing::typeck(
        global_ctxt.typing_context(),
        &global_ctxt.common_types,
        parsed_tree,
        parsed_interface,
        global_ctxt.typed_interface(),
    )
    .unwrap();
    let knorm_tree = ir_knorm::lowering(global_ctxt.knorm_context(), typed_tree);
    let closure_prog = ir_closure::lowering(global_ctxt.closure_context(), knorm_tree);
    let wasm_bytes = codegen_wasm::codegen(closure_prog, global_ctxt.typed_interface()).unwrap();
    std::fs::File::create(global_ctxt.session().output_path.as_ref().unwrap())
        .unwrap()
        .write_all(&wasm_bytes)
        .unwrap();
}
