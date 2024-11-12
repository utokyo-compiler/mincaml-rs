use std::fmt;

use sourcemap::{Loc, Spanned};
use syntax::{Expr, ExprKind, Mli};

use crate::{
    context::Context,
    lexer::{self, Lexer, Token},
};

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
#[allow(clippy::all)]
mod mli;
#[cfg(feature = "lalrpop")]
pub type SelectedParser = lalrpop::LalrpopParser;

/// Assertion that the selected parser implements `Parser`.
const _: () = {
    use std::marker::PhantomData;
    struct IsParser<T: Parser>(PhantomData<T>);
    let _ = IsParser::<SelectedParser>(PhantomData);
};

pub trait Parser {
    fn parse<'input, 'ctx>(
        alloc: Allocator<'ctx>,
        lexer: impl Lexer<'input>,
    ) -> Result<Expr<'ctx>, Error<'input>>;

    fn parse_mli<'input, 'ctx>(
        alloc: Allocator<'ctx>,
        lexer: impl Lexer<'input>,
    ) -> Result<Mli<'ctx>, Error<'input>>;
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

#[derive(Clone, Copy)]
pub(crate) struct Allocator<'ctx> {
    ctx: &'ctx Context<'ctx>,
}

impl<'ctx> Allocator<'ctx> {
    pub(crate) fn new(ctx: &'ctx Context<'ctx>) -> Self {
        Self { ctx }
    }

    #[inline(always)]
    pub(crate) fn spanned(&self, expr: ExprKind<'ctx>, span: (Loc, Loc)) -> Expr<'ctx> {
        self.ctx.new_expr(Spanned::new(expr, span))
    }

    pub(crate) fn ctx(&self) -> &Context<'ctx> {
        self.ctx
    }

    #[inline]
    /// Breaks down a [`CtorTranspose`] into an [`Expr`]. This function helps to *lie* to the parser generator
    /// about operator precedence in order to avoid action conflicts.
    pub(crate) fn apply(&self, trans: CtorTranspose<'ctx>) -> Expr<'ctx> {
        trans.apply_all(self)
    }
}

#[allow(unused)]
/// Defunctionalization of `(Expr<'ctx>, Loc) -> Expr<'ctx>`.
pub(crate) enum CtorContinuation<'ctx> {
    /// Delayed [`ExprKind::Set`] creation.
    ///
    /// ```ignore (illustrative)
    /// |e2: Expr<'ctx>, r: Loc| alloc.spanned(Set(e1, e2), (l, r))
    /// ```
    DelayedSet { e1: Expr<'ctx>, l: Loc },

    /// Delayed [`ExprKind::If`] creation.
    ///
    /// ```ignore (illustrative)
    /// |e3: Expr<'ctx>, r: Loc| alloc.spanned(If(e1, e2, e3), (l, r))
    /// ```
    DelayedIf {
        e1: Expr<'ctx>,
        e2: Expr<'ctx>,
        l: Loc,
    },
}

impl<'ctx> CtorContinuation<'ctx> {
    fn apply_expr(self, e: Expr<'ctx>, r: Loc, alloc: &Allocator<'ctx>) -> Expr<'ctx> {
        match self {
            CtorContinuation::DelayedSet { e1, l } => alloc.spanned(ExprKind::Set(e1, e), (l, r)),
            CtorContinuation::DelayedIf { e1, e2, l } => {
                alloc.spanned(ExprKind::If(e1, e2, e), (l, r))
            }
        }
    }
}

#[allow(unused)]
pub(crate) struct CtorTranspose<'ctx> {
    continuations: Vec<CtorContinuation<'ctx>>,
    kind: CtorTransposeKind<'ctx>,
}

impl<'ctx> CtorTranspose<'ctx> {
    fn new(kind: CtorTransposeKind<'ctx>) -> Self {
        Self {
            continuations: Vec::new(),
            kind,
        }
    }
    pub(crate) fn new_eval(e: Expr<'ctx>, r: Loc) -> Self {
        Self::new(CtorTransposeKind::Eval { e, r })
    }
    pub(crate) fn new_delayed_then(
        e1: Expr<'ctx>,
        r: Loc,
        e2: Expr<'ctx>,
        l: Loc,
        r2: Loc,
    ) -> Self {
        Self::new(CtorTransposeKind::DelayedThen { e1, r, e2, l, r2 })
    }
    pub(crate) fn composed_left(mut self, cont: CtorContinuation<'ctx>) -> Self {
        self.continuations.push(cont);
        self
    }
    fn apply_all(self, alloc: &Allocator<'ctx>) -> Expr<'ctx> {
        // optimization: if there are no continuations, we can avoid the fold
        if self.continuations.is_empty() {
            return self.kind.apply_func(|e, _, _| e, alloc);
        }
        let f = move |e: Expr<'ctx>, r: Loc, alloc: &Allocator<'ctx>| {
            self.continuations
                .into_iter()
                .fold(e, |e, cont| cont.apply_expr(e, r, alloc))
        };
        self.kind.apply_func(f, alloc)
    }
}

#[allow(unused)]
/// Defunctionalization of `((Expr<'ctx>, Loc) -> Expr<'ctx>) -> Expr<'ctx>`.
pub(crate) enum CtorTransposeKind<'ctx> {
    /// Eval function.
    ///
    /// ```ignore (illustrative)
    /// |f: fn(Expr<'ctx>, Loc) -> Expr<'ctx>| f(e, r)
    /// ```
    Eval { e: Expr<'ctx>, r: Loc },

    /// Delayed [`ExprKind::Then`] creation. `alloc` in the following pseudo code is provided by the caller.
    ///
    /// ```ignore (illustrative)
    /// |f: fn(Expr<'ctx>, Loc) -> Expr<'ctx>| {
    ///     let e1 = f(e1, r);
    ///     alloc.spanned(Then(e1, e2), (l, r2))
    /// }
    /// ```
    DelayedThen {
        e1: Expr<'ctx>,
        r: Loc,
        e2: Expr<'ctx>,
        l: Loc,
        r2: Loc,
    },
}

impl<'ctx> CtorTransposeKind<'ctx> {
    #[allow(unused)]
    #[inline(always)]
    /// Apply a function to the value.
    fn apply_func<'alloc>(
        self,
        f: impl FnOnce(Expr<'ctx>, Loc, &'alloc Allocator<'ctx>) -> Expr<'ctx>,
        alloc: &'alloc Allocator<'ctx>,
    ) -> Expr<'ctx> {
        match self {
            CtorTransposeKind::Eval { e, r } => f(e, r, alloc),
            CtorTransposeKind::DelayedThen { e1, e2, l, r, r2 } => {
                let e1 = f(e1, r, alloc);
                alloc.spanned(ExprKind::Then(e1, e2), (l, r2))
            }
        }
    }
}
