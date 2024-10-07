pub mod mli;

use data_structure::{
    arena::Box,
    index::{vec::IndexVec, Indexable},
};
use sourcemap::{Span, Spanned};
pub use syntax::{BinOp, FloatBinOpKind, IntBinOpKind, LitKind, RelationBinOpKind, UnOp};
pub use ty::{ArgIndex, TupleIndex, Ty, TyKind, Typed};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Represents a resolved identifier.
pub enum DisambiguatedIdent<'ctx> {
    UserDefined {
        name: syntax::Ident<'ctx>,
        span: Span,
    },

    /// An intrinsic function.
    Intrinsic { name: syntax::Ident<'ctx> },

    CompilerGenerated {
        name: &'static str,

        /// A number to distinguish between the same name.
        ///
        /// It is possible to have the same value for different names.
        disambiguator: usize,
    },
}

impl<'ctx> DisambiguatedIdent<'ctx> {
    pub fn new_user(name: syntax::Ident<'ctx>, span: Span) -> Self {
        Self::UserDefined { name, span }
    }

    pub fn new_intrinsic(name: syntax::Ident<'ctx>) -> Self {
        Self::Intrinsic { name }
    }

    pub const fn new_compiler_unchecked(name: &'static str, disambiguator: usize) -> Self {
        Self::CompilerGenerated {
            name,
            disambiguator,
        }
    }
}

impl<'ctx> std::fmt::Display for DisambiguatedIdent<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UserDefined { name, .. } => write!(f, "{}", name.0),
            Self::CompilerGenerated {
                name,
                disambiguator,
            } => {
                write!(f, "__{name}#{{{disambiguator}}}")
            }
            Self::Intrinsic { name } => write!(f, "{}", name.0),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident<'ctx>(Box<'ctx, TypedIdent<'ctx>>);

impl<'ctx> Ident<'ctx> {
    pub fn new(interned: Box<'ctx, TypedIdent<'ctx>>) -> Self {
        Self(interned)
    }
}

impl<'ctx> std::ops::Deref for Ident<'ctx> {
    type Target = Box<'ctx, TypedIdent<'ctx>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'ctx> std::ops::DerefMut for Ident<'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub type TypedIdent<'ctx> = Typed<'ctx, DisambiguatedIdent<'ctx>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Expr<'ctx>(Box<'ctx, TypedExprKind<'ctx>>);

impl<'ctx> Expr<'ctx> {
    pub fn new(interned: Box<'ctx, TypedExprKind<'ctx>>) -> Self {
        Self(interned)
    }
}

impl<'ctx> std::ops::Deref for Expr<'ctx> {
    type Target = Box<'ctx, TypedExprKind<'ctx>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'ctx> std::ops::DerefMut for Expr<'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub type TypedExprKind<'ctx> = Typed<'ctx, Spanned<ExprKind<'ctx>>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Expr<'ctx>),
    Binary(BinOp, Expr<'ctx>, Expr<'ctx>),
    If(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinding<'ctx>, Expr<'ctx>),
    Then(Expr<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    App(Expr<'ctx>, IndexVec<ArgIndex, Expr<'ctx>>),
    Tuple(IndexVec<TupleIndex, Expr<'ctx>>),
    ArrayMake(Expr<'ctx>, Expr<'ctx>),
    Get(Expr<'ctx>, Expr<'ctx>),
    Set(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
}
impl Indexable<ArgIndex> for Expr<'_> {}
impl Indexable<TupleIndex> for Expr<'_> {}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<'ctx> {
    pub pattern: Pattern<'ctx>,
    pub args: IndexVec<ArgIndex, Ident<'ctx>>,
    pub value: Expr<'ctx>,
}
impl Indexable<ArgIndex> for Ident<'_> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Var(Ident<'ctx>),
    Tuple(IndexVec<TupleIndex, Ident<'ctx>>),
}
impl Indexable<TupleIndex> for Ident<'_> {}
