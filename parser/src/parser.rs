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

/// Assertion that the selected parser implements `Parser`.
const _: () = {
    use std::marker::PhantomData;
    struct IsParser<T: Parser>(PhantomData<T>);
    let _ = IsParser::<SelectedParser>(PhantomData);
};

pub trait Parser {
    fn parse<'input, 'arena: 'input>(
        bump: &'arena Bump,
        lexer: impl Lexer<'input>,
    ) -> Result<Expr<'input, 'arena>, Error<'input>>;
}

#[derive(Debug, Clone)]
pub enum Error<'input> {
    LexError(lexer::Error<'input>),
    ParseError(ParseError<'input>),
}

#[derive(Debug, Clone)]
pub enum ParseError<'input> {
    UnrecognizedEof(Loc),
    InvalidToken(Loc),
    ExtraToken(Spanned<Token<'input>>),
    UnexpectedToken(Spanned<Token<'input>>, ExpectedTokens),
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
