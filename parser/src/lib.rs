use syntax::Expr;

mod lexer;
mod parser;

pub type Error<'a> = parser::Error<'a>;

pub fn lex_and_parse(input: &str) -> Result<Expr<'_>, Error<'_>> {
    use lexer::Lexer;
    use parser::Parser;

    parser::SelectedParser::parse(lexer::SelectedLexer::new(input))
}
