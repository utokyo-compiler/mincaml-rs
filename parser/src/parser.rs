use sourcemap::Spanned;
use syntax::Expr;

use crate::lexer::{self, Lexer, Token};

pub trait Parser {
    fn parse<'t>(lexer: impl Lexer<'t>) -> Result<Expr<'t>, Error<'t>>;
}

#[derive(Debug, Clone)]
pub enum Error<'a> {
    LexError(lexer::Error<'a>),
    ParseError(ParseError<'a>),
}

#[derive(Debug, Clone)]
pub enum ParseError<'t> {
    UnexpectedToken(Spanned<Token<'t>>, peg::error::ExpectedSet),
}
