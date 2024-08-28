use bumpalo::Bump;
use syntax::Expr;

mod lexer;
mod parser;

pub type Error<'a> = parser::Error<'a>;

pub fn lex_and_parse<'input, 'alloc: 'input>(
    bump: &'alloc Bump,
    input: &'input str,
) -> Result<Expr<'input, 'alloc>, Error<'input>> {
    use lexer::Lexer;
    use parser::Parser;

    parser::SelectedParser::parse(bump, lexer::SelectedLexer::new(input))
}
