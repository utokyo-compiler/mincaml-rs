use std::str::FromStr;

use sourcemap::Spanned;
use thiserror::Error;

#[cfg(feature = "plex")]
mod plex;
#[cfg(feature = "plex")]
pub type SelectedLexer<'input> = plex::PlexLexer<'input>;

/// Assertion that the selected lexer implements `Lexer`.
const _: () = {
    use std::marker::PhantomData;
    struct AssertIsLexer<'input, T: Lexer<'input>>(PhantomData<&'input T>);
    let _ = AssertIsLexer::<SelectedLexer>(PhantomData);
};

pub trait Lexer<'input>: Iterator<Item = Result<'input, Spanned<Token<'input>>>> {
    fn new(input: &'input str) -> Self;

    #[allow(unused)]
    fn read_to_vec(self) -> Result<'input, Vec<Spanned<Token<'input>>>>
    where
        Self: Sized,
    {
        self.collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'input> {
    Int(i32),
    Bool(bool),
    Float(f32),
    Ident(&'input str),
    StringLiteral,
    ItemAttr,
    Hyphen,
    Plus,
    Ast,
    Slash,
    HyphenDot,
    PlusDot,
    AstDot,
    SlashDot,
    Equal,
    LessGreater,
    LessEqual,
    LessHyphen,
    HyphenGreater,
    GreaterEqual,
    Less,
    Greater,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    Val,
    External,
    Comma,
    Colon,
    ArrayMake,
    Dot,
    Semi,
    LPar,
    RPar,
}

pub type Error<'input> = Spanned<ErrorKind<'input>>;

#[derive(Debug, Clone, Error)]
pub enum ErrorKind<'input> {
    #[error("reached EOF before comment closes")]
    UnclosedComment,
    #[error("unrecognized token `{0}`")]
    UnrecognizedToken(&'input str),
    #[error("cannot parse `{0}` as an integer constant: {1}")]
    IllegalIntegerConstant(&'input str, ParseIntegerErr),
    #[error("cannot parse `{0}` as a float constant: {1}")]
    IllegalFloatConstant(&'input str, <f32 as FromStr>::Err),
}

#[derive(Debug, Clone, Error)]
pub enum ParseIntegerErr {
    #[error(transparent)]
    ParseIntError(std::num::ParseIntError),
    #[error("leading zeros are not allowed")]
    LeadingZero,
}

pub type Result<'input, T> = std::result::Result<T, Error<'input>>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let l = sourcemap::Loc::new;
        let r = SelectedLexer::new("1 + 2 * 3").read_to_vec();
        assert_eq!(
            r.unwrap_or_else(|e| panic!("{e:#?}")),
            vec![
                Spanned::new(Token::Int(1), (l(0), l(1))),
                Spanned::new(Token::Plus, (l(2), l(3))),
                Spanned::new(Token::Int(2), (l(4), l(5))),
                Spanned::new(Token::Ast, (l(6), l(7))),
                Spanned::new(Token::Int(3), (l(8), l(9))),
            ]
        );
        let r = SelectedLexer::new("00").read_to_vec();
        assert!(r.is_err(), "{r:#?}");
        let r = SelectedLexer::new("0. 2").read_to_vec();
        assert_eq!(
            r.unwrap_or_else(|e| panic!("{e:#?}")),
            vec![
                Spanned::new(Token::Float(0.), (l(0), l(2))),
                Spanned::new(Token::Int(2), (l(3), l(4))),
            ]
        );
    }
}
