use syntax::Expr;

mod lexer;
mod parser;

#[derive(Debug, Clone)]
pub enum Error<'a> {
    LexError(lexer::Error<'a>),
    ParseError(parser::Error<'a>),
}

pub fn lex_and_parse(input: &str) -> Result<Expr<'_>, Error<'_>> {
    let lexer = lexer::Lexer::new(input);
    let tokens = lexer.read_to_vec().map_err(Error::LexError)?;
    parser::parse(&tokens).map_err(Error::ParseError)
}
