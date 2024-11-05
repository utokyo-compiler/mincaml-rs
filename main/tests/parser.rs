fn parse_input(input: &str) {
    let input = sourcemap::InputFile::String {
        content: input.to_string(),
    };
    let mut ml_input = sourcemap::MultipleInput::new();
    ml_input.add_file(input);
    let mli_input = sourcemap::MultipleInput::default();

    let arena = middleware::Arena::default();
    let session = session::Session::new(
        ml_input,
        mli_input,
        None,
        session::CompilerOption::default(),
    );
    let global_ctxt = middleware::GlobalContext::new(&arena, session);

    let input = global_ctxt.session().input.concatenated_string();
    let _parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), &input).unwrap();
}

#[test]
fn test_parse_input() {
    parse_input("let x = 1 in x");
}

fn parse_input_mli(input: &str) {
    let input = sourcemap::InputFile::String {
        content: input.to_string(),
    };
    let ml_input = sourcemap::MultipleInput::new();
    let mut mli_input = sourcemap::MultipleInput::default();
    mli_input.add_file(input);

    let arena = middleware::Arena::default();
    let session = session::Session::new(
        ml_input,
        mli_input,
        None,
        session::CompilerOption::default(),
    );
    let global_ctxt = middleware::GlobalContext::new(&arena, session);

    let input = global_ctxt.session().input_interface.concatenated_string();
    let _parsed_tree = parser::lex_and_parse_mli(global_ctxt.parsing_context(), &input).unwrap();
}

#[test]
fn test_parse_mli() {
    parse_input_mli(
        r#"
val fispos : float -> bool
val fisneg : float -> bool

external (<>) : int -> int -> bool = "%notequal"
external ( * ) : int -> int -> int = "%mulint"

external cos : float -> float = "cos_float" "cos" [@@unboxed] [@@noalloc] (* "float" *)
"#,
    );
}
