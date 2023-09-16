use bumpalo::Bump;
use syntax::Expr;

mod lexer;
mod parser;

pub type Error<'a> = parser::Error<'a>;

pub fn lex_and_parse<'input, 'b: 'input>(
    bump: &'b Bump,
    input: &'input str,
) -> Result<Expr<'input, 'b>, Error<'input>> {
    use lexer::Lexer;
    use parser::Parser;

    parser::SelectedParser::parse(bump, lexer::SelectedLexer::new(input))
}
