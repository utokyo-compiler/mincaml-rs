use std::str::FromStr;

use sourcemap::Spanned;
use thiserror::Error;

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
pub enum Token<'a> {
    Int(i32),
    Bool(bool),
    Float(f32),
    Ident(&'a str),
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

pub type Error<'a> = Spanned<ErrorKind<'a>>;

#[derive(Debug, Clone, Error)]
pub enum ErrorKind<'a> {
    #[error("reached EOF before comment closes")]
    UnclosedComment,
    #[error("unrecognized token `{0}`")]
    UnrecognizedToken(&'a str),
    #[error("cannot parse `{0}` as integer constant: {1}")]
    IllegalIntegerConstant(&'a str, <i32 as FromStr>::Err),
    #[error("cannot parse `{0}` as float constant: {1}")]
    IllegalFloatConstant(&'a str, <f32 as FromStr>::Err),
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
