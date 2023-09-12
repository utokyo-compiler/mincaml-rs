use std::str::FromStr;

use plex::lexer;
use thiserror::Error;

use sourcemap::{Loc, Spanned};

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

#[derive(Debug, Clone, PartialEq, Error)]
pub enum Error<'a> {
    #[error("reached EOF before comment closes")]
    UnclosedComment,
    #[error("unrecognized token `{0}`")]
    UnrecognizedToken(&'a str),
    #[error("cannot parse `{0}` as integer constant: {1}")]
    IllegalIntegerConstant(&'a str, <i32 as FromStr>::Err),
    #[error("cannot parse `{0}` as float constant: {1}")]
    IllegalFloatConstant(&'a str, <f32 as FromStr>::Err),
}

type Result<'a, T> = std::result::Result<T, Spanned<Error<'a>>>;

enum CommentState {
    Open,
    Close,
    Continue,
}

lexer! {
    fn consume_comment(_text: 'input) -> CommentState;

    r"\(\*" => CommentState::Open,
    r"\*\)" => CommentState::Close,
    r"." => CommentState::Continue
}

enum LexState<'a> {
    Token(Token<'a>),
    Skip,
    CommentBegin,
    Error(Error<'a>),
}

lexer! {
    fn next_token(text: 'input) -> LexState<'input>;

    r"\(\*" => LexState::CommentBegin,
    r"[\t\r ]" => LexState::Skip,
    r"\(" => LexState::Token(Token::LPar),
    r"\)" => LexState::Token(Token::RPar),
    r"true" => LexState::Token(Token::Bool(true)),
    r"false" => LexState::Token(Token::Bool(false)),
    r"not" => LexState::Token(Token::Not),
    r"0|[1-9][0-9]*" => {
        match text.parse() {
            Ok(i) => LexState::Token(Token::Int(i)),
            Err(e) => LexState::Error(Error::IllegalIntegerConstant(text, e)),
        }
    },
    r"0|[1-9][0-9]*\.[0-9]*([eE][+-]?[0-9]+)?" => {
        match text.parse() {
            Ok(i) => LexState::Token(Token::Float(i)),
            Err(e) => LexState::Error(Error::IllegalFloatConstant(text, e)),
        }
    },
    r"\-" => LexState::Token(Token::Hyphen),
    r"\+" => LexState::Token(Token::Plus),
    r"\*" => LexState::Token(Token::Ast),
    r"/" => LexState::Token(Token::Slash),
    r"\-\." => LexState::Token(Token::HyphenDot),
    r"\+\." => LexState::Token(Token::PlusDot),
    r"\*\." => LexState::Token(Token::AstDot),
    r"/\." => LexState::Token(Token::SlashDot),
    r"=" => LexState::Token(Token::Equal),
    r"<>" => LexState::Token(Token::LessGreater),
    r"<=" => LexState::Token(Token::LessEqual),
    r">=" => LexState::Token(Token::GreaterEqual),
    r"<" => LexState::Token(Token::Less),
    r">" => LexState::Token(Token::Greater),
    r"if" => LexState::Token(Token::If),
    r"then" => LexState::Token(Token::Then),
    r"else" => LexState::Token(Token::Else),
    r"let" => LexState::Token(Token::Let),
    r"in" => LexState::Token(Token::In),
    r"rec" => LexState::Token(Token::Rec),
    r"," => LexState::Token(Token::Comma),
    r"Array\.make" => LexState::Token(Token::ArrayMake),
    r"\." => LexState::Token(Token::Dot),
    r"<\-" => LexState::Token(Token::LessHyphen),
    r";" => LexState::Token(Token::Semi),
    r"[a-z_][0-9A-Za-z_]*" => LexState::Token(Token::Ident(text)),
    r"." => LexState::Error(Error::UnrecognizedToken(text))
}

#[derive(Debug, Clone)]
pub struct Lexer<'input> {
    remain: &'input str,
    last_remain_len: usize,
    current_loc: Loc<usize>,
}

impl<'input> Lexer<'input> {
    pub fn new(s: &'input str) -> Self {
        Lexer {
            remain: s,
            last_remain_len: s.chars().count(),
            current_loc: Loc { char_pos: 0 },
        }
    }
    pub fn read_to_vec(self) -> Result<'input, Vec<Spanned<Token<'input>>>> {
        self.collect()
    }

    fn skip_comment(&mut self) -> std::result::Result<(), Error<'input>> {
        let mut depth = 0;
        loop {
            let state = consume_comment(self.remain);
            if state.is_none() {
                // EOF
                return Err(Error::UnclosedComment);
            }

            let (state, remaining) = state.unwrap();

            self.remain = remaining;

            let remain_len = remaining.chars().count();
            let consumed = self.last_remain_len - remain_len;
            self.last_remain_len = remain_len;

            self.current_loc.char_pos += consumed;

            use CommentState::*;

            match state {
                Open => {
                    depth += 1;
                }
                Close => {
                    if depth == 0 {
                        return Ok(());
                    } else {
                        depth -= 1;
                    }
                }
                Continue => {
                    const CONCERN: &[char; 3] = &['(', ')', '*'];
                    self.remain = self
                        .remain
                        .trim_matches(|c| CONCERN.binary_search(&c).is_err());
                    continue;
                }
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<'input, Spanned<Token<'input>>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let state = next_token(self.remain);

            let (state, remaining) = state?;

            self.remain = remaining;

            let remain_len = remaining.chars().count();
            let consumed = self.last_remain_len - remain_len;
            self.last_remain_len = remain_len;

            let lo = self.current_loc.clone();
            self.current_loc.char_pos += consumed;
            let hi = self.current_loc.clone();
            let span = (lo, hi);

            use LexState::*;

            match state {
                Token(tok) => return Some(Ok(Spanned::new(tok, span))),
                Skip => continue,
                Error(e) => return Some(Err(Spanned::new(e, span))),
                CommentBegin => {
                    if let Err(e) = self.skip_comment() {
                        return Some(Err(Spanned::new(e, span)));
                    }
                }
            }
        }
    }
}
