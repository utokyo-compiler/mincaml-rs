use crate::{
    lexer::{Error as LexError, Lexer, Token},
    parser::{Error, ExpectedTokens, ParseError, Parser},
};
use sourcemap::{Loc, Spanned};

use super::{mincaml, mli, Allocator};

pub struct LalrpopParser;

impl Parser for LalrpopParser {
    fn parse<'input, 'ctx>(
        alloc: Allocator<'ctx>,
        lexer: impl Lexer<'input>,
    ) -> Result<syntax::Expr<'ctx>, crate::parser::Error<'input>> {
        let lexer = LexerWrapper::new(lexer);
        let parser = mincaml::ExprParser::new();

        parser
            .parse(alloc, lexer)
            .map_err(Error::from_lalrpop_error)
    }

    fn parse_mli<'input, 'ctx>(
        alloc: Allocator<'ctx>,
        lexer: impl Lexer<'input>,
    ) -> Result<syntax::Mli<'ctx>, Error<'input>> {
        let lexer = LexerWrapper::new(lexer);
        let parser = mli::MliParser::new();

        parser
            .parse(alloc, lexer)
            .map_err(Error::from_lalrpop_error)
    }
}

struct LexerWrapper<'input, L: Lexer<'input>> {
    _marker: std::marker::PhantomData<&'input ()>,
    lexer: L,
}

impl<'input, L: Lexer<'input>> Iterator for LexerWrapper<'input, L> {
    type Item = Result<(Loc, Token<'input>, Loc), LexError<'input>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|res| {
            res.map(|spanned| {
                let user_defined_span = spanned.span.as_user_defined().unwrap();
                (user_defined_span.start, spanned.node, user_defined_span.end)
            })
        })
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

impl<'input> Error<'input> {
    fn from_lalrpop_error(err: LalrpopError<'input>) -> Self {
        Self::ParseError(match err {
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
}
