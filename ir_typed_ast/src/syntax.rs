use data_structure::arena::Box;
use sourcemap::{Span, Spanned};
pub use syntax::{BinOp, LitKind, UnOp};
pub use ty::{Ty, Typed};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Represents a resolved identifier.
pub enum DisambiguatedIdent<'ctx> {
    UserDefined {
        name: syntax::Ident<'ctx>,
        span: Span,
    },
    CompilerGenerated {
        name: &'static str,
        disambiguator: usize,
    },
}

impl<'ctx> DisambiguatedIdent<'ctx> {
    pub fn new_user(name: syntax::Ident<'ctx>, span: Span) -> Self {
        Self::UserDefined { name, span }
    }
    pub fn new_compiler_unchecked(name: &'static str, disambiguator: usize) -> Self {
        Self::CompilerGenerated {
            name,
            disambiguator,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IdentOrigin {
    UserDefined,
    CompilerGenerated,
}

pub type Ident<'ctx> = Box<'ctx, TypedIdent<'ctx>>;
pub type TypedIdent<'ctx> = Typed<'ctx, DisambiguatedIdent<'ctx>>;

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
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
    App(Expr<'ctx>, Vec<Expr<'ctx>>),
    Tuple(Vec<Expr<'ctx>>),
    ArrayMake(Expr<'ctx>, Expr<'ctx>),
    Get(Expr<'ctx>, Expr<'ctx>),
    Set(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<'ctx> {
    pub place: Pattern<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub value: Expr<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Var(Ident<'ctx>),
    Tuple(Vec<Ident<'ctx>>),
}
