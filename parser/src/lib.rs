use syntax::Expr;

mod lexer;
#[cfg(feature = "plex")]
mod plex;

mod parser;

#[derive(Debug, Clone)]
pub enum Error<'a> {
    LexError(lexer::Error<'a>),
    ParseError(parser::Error<'a>),
}

#[cfg(all(feature = "plex", feature = "peg"))]
pub fn lex_and_parse(input: &str) -> Result<Expr<'_>, Error<'_>> {
    use lexer::Lexer;

    let lexer = plex::PlexLexer::new(input);
    let tokens = lexer.read_to_vec().map_err(Error::LexError)?;
    parser::parse(&tokens).map_err(Error::ParseError)
}
