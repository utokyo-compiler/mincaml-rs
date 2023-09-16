use bumpalo::Bump;

fn main() {
    let input = "todo";
    let bump = Bump::new();
    let expr = parser::lex_and_parse(&bump, input).unwrap();
    todo!()
}
