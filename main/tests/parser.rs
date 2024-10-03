fn parse_input(input: &str) {
    let arena = middleware::Arena::default();
    let compiler_option = middleware::CompilerOption::default();
    let global_ctxt = middleware::GlobalContext::new(&arena, compiler_option);

    let _parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), input).unwrap();
}

#[test]
fn test_parse_input() {
    parse_input("let x = 1 in x");
}

fn parse_input_mli(input: &str) {
    let arena = middleware::Arena::default();
    let compiler_option = middleware::CompilerOption::default();
    let global_ctxt = middleware::GlobalContext::new(&arena, compiler_option);

    let _parsed_tree = parser::lex_and_parse_mli(global_ctxt.parsing_context(), input).unwrap();
}

#[test]
fn test_parse_mli() {
    parse_input_mli(
        r#"
val fispos : float -> bool
val fisneg : float -> bool

external (<>) : int -> int -> bool = "%notequal"
external ( * ) : int -> int -> int = "%mulint"

external cos : float -> float = "cos_float" "cos" "float"
"#,
    );
}
