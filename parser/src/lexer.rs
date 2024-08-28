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

    fn read_to_vec(self) -> Result<'input, Vec<Spanned<Token<'input>>>>
    where
        Self: Sized,
    {
        self.collect()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Token<'input> {
    Int(i32),
    Bool(bool),
    Float(f32),
    Ident(&'input str),
    Not,
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
    GreaterEqual,
    Less,
    Greater,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    Comma,
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
    IllegalIntegerConstant(&'input str, <i32 as FromStr>::Err),
    #[error("cannot parse `{0}` as a float constant: {1}")]
    IllegalFloatConstant(&'input str, <f32 as FromStr>::Err),
}

pub type Result<'input, T> = std::result::Result<T, Error<'input>>;
