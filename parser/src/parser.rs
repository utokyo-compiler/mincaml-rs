use std::fmt;

use bumpalo::Bump;
use sourcemap::{Loc, Spanned};
use syntax::Expr;

use crate::lexer::{self, Lexer, Token};

#[cfg(feature = "peg")]
mod peg;
#[cfg(feature = "peg")]
pub type SelectedParser = peg::PegParser;

#[cfg(feature = "lalrpop")]
mod lalrpop;
#[cfg(feature = "lalrpop")]
#[allow(clippy::all)]
mod mincaml;
#[cfg(feature = "lalrpop")]
pub type SelectedParser = lalrpop::LalrpopParser;

pub trait Parser {
    fn parse<'t, 'b: 't>(bump: &'b Bump, lexer: impl Lexer<'t>) -> Result<Expr<'t, 'b>, Error<'t>>;
}

#[derive(Debug, Clone)]
pub enum Error<'a> {
    LexError(lexer::Error<'a>),
    ParseError(ParseError<'a>),
}

#[derive(Debug, Clone)]
pub enum ParseError<'t> {
    UnrecognizedEof(Loc),
    InvalidToken(Loc),
    ExtraToken(Spanned<Token<'t>>),
    UnexpectedToken(Spanned<Token<'t>>, ExpectedTokens),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedTokens(pub Vec<String>);

impl fmt::Display for ExpectedTokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = &self.0;
        match v.len() {
            0 => unreachable!("in this case ExtraToken should be thrown"),
            1 => write!(f, "{}", v[0]),
            _ => write!(f, "one of [{}]", v.join(", ")),
        }
    }
}
