use crate::{
    lexer::{Error as LexError, Lexer, Token},
    parser::{Error, ExpectedTokens, ParseError, Parser},
};
use sourcemap::{Loc, Spanned};

use super::mincaml;

pub struct LalrpopParser;

impl Parser for LalrpopParser {
    fn parse<'t>(lexer: impl Lexer<'t>) -> Result<syntax::Expr<'t>, crate::parser::Error<'t>> {
        let lexer = LexerWrapper::new(lexer);
        let parser = mincaml::ExprParser::new();

        parser.parse(lexer).map_err(convert_error)
    }
}

struct LexerWrapper<'input, L: Lexer<'input>> {
    _marker: std::marker::PhantomData<&'input ()>,
    lexer: L,
}

impl<'input, L: Lexer<'input>> Iterator for LexerWrapper<'input, L> {
    type Item = Result<(Loc, Token<'input>, Loc), LexError<'input>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer
            .next()
            .map(|res| res.map(|spanned| (spanned.span.start, spanned.node, spanned.span.end)))
    }
}

impl<'input, L: Lexer<'input>> LexerWrapper<'input, L> {
    fn new(lexer: L) -> Self {
        Self {
            _marker: std::marker::PhantomData,
            lexer,
        }
    }
}

type LalrpopError<'a> = lalrpop_util::ParseError<Loc, Token<'a>, LexError<'a>>;

fn convert_error(err: LalrpopError) -> Error {
    Error::ParseError(match err {
        LalrpopError::InvalidToken { location } => ParseError::InvalidToken(location),
        LalrpopError::ExtraToken {
            token: (lo, tok, hi),
        } => ParseError::ExtraToken(Spanned::new(tok, (lo, hi))),
        LalrpopError::User { error } => return Error::LexError(error),
        LalrpopError::UnrecognizedToken {
            token: (lo, tok, hi),
            expected,
        } => ParseError::UnexpectedToken(Spanned::new(tok, (lo, hi)), ExpectedTokens(expected)),
        LalrpopError::UnrecognizedEof { location, .. } => ParseError::UnrecognizedEof(location),
    })
}
