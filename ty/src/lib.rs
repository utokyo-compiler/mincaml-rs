pub mod context;

use context::TypingContext;
use data_structure::{
    index::{
        vec::{Idx, IndexVec},
        Indexable,
    },
    interning::Interned,
};
use errors::into_diag_arg_using_display;

/// Use this instead of `TyKind` whenever possible.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty<'ctx>(pub Interned<'ctx, TyKind<'ctx>>);

impl<'ctx> std::ops::Deref for Ty<'ctx> {
    type Target = TyKind<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'ctx> std::fmt::Display for Ty<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ArgIndex(usize);
impl Idx for ArgIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TupleIndex(usize);
impl Idx for TupleIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

impl Indexable<ArgIndex> for Ty<'_> {}
impl Indexable<TupleIndex> for Ty<'_> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<'ctx> {
    Unit,
    Bool,
    Int,
    Float,
    Fun(IndexVec<ArgIndex, Ty<'ctx>>, Ty<'ctx>),
    Tuple(IndexVec<TupleIndex, Ty<'ctx>>),
    Array(Ty<'ctx>),

    /// A type variable.
    ///
    /// Used in type checking, not allowed after that.
    TyVar(TyVarId),
}

impl<'ctx> TyKind<'ctx> {
    pub fn as_ty_var(&self) -> Option<TyVarId> {
        if let Self::TyVar(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_fun_ty(&self) -> Option<(&[Ty<'ctx>], Ty<'ctx>)> {
        if let Self::Fun(args, ret) = self {
            Some((args.as_slice(), *ret))
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<Ty<'ctx>> {
        if let Self::Array(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

impl<'ctx> std::fmt::Display for TyKind<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn into_safe(ty: String) -> String {
            if (ty.contains("->") || ty.contains("*")) && !ty.starts_with("(") {
                format!("({ty})")
            } else {
                ty
            }
        }
        match self {
            TyKind::Unit => write!(f, "()"),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Int => write!(f, "int"),
            TyKind::Float => write!(f, "float"),
            TyKind::Fun(args, ret) => {
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, " -> ")?;
                    }
                    write!(f, "{}", into_safe(arg.to_string()))?;
                }
                write!(f, " -> {ret}")
            }
            TyKind::Tuple(types) => {
                let mut first = true;
                for ty in types {
                    if first {
                        first = false;
                    } else {
                        write!(f, " * ")?;
                    }
                    write!(f, "{}", into_safe(ty.to_string()))?;
                }
                Ok(())
            }
            TyKind::Array(ty) => write!(f, "{} array", into_safe(ty.to_string())),
            TyKind::TyVar(ty_var_id) => write!(f, "{ty_var_id}"),
        }
    }
}

/// Used in type checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVarId(usize);

impl std::fmt::Display for TyVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'_weak{}", self.0)
    }
}

impl TyVarId {
    pub fn new_unchecked(id: usize) -> Self {
        Self(id)
    }
}

impl<'ctx> Ty<'ctx> {
    pub fn new<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>, kind: TyKind<'ctx>) -> Self {
        ctx.mk_ty_from_kind(kind)
    }

    pub fn kind(self) -> &'ctx TyKind<'ctx> {
        self.0 .0
    }

    pub fn is_unit(self) -> bool {
        matches!(self.kind(), TyKind::Unit)
    }

    #[inline(always)]
    pub fn mk_unit<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Unit)
    }

    #[inline(always)]
    pub fn mk_bool<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Bool)
    }

    #[inline(always)]
    pub fn mk_int<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Int)
    }

    #[inline(always)]
    pub fn mk_float<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Float)
    }

    #[inline(always)]
    pub fn mk_fun<Ident, Expr>(
        ctx: &TypingContext<'ctx, Ident, Expr>,
        args: Vec<Ty<'ctx>>,
        ret: Ty<'ctx>,
    ) -> Self {
        Self::new(ctx, TyKind::Fun(IndexVec::from_raw_vec(args), ret))
    }

    #[inline(always)]
    pub fn mk_tuple<Ident, Expr>(
        ctx: &TypingContext<'ctx, Ident, Expr>,
        tys: Vec<Ty<'ctx>>,
    ) -> Self {
        Self::new(ctx, TyKind::Tuple(IndexVec::from_raw_vec(tys)))
    }

    #[inline(always)]
    pub fn mk_array<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>, ty: Ty<'ctx>) -> Self {
        Self::new(ctx, TyKind::Array(ty))
    }

    #[inline(always)]
    pub fn mk_ty_var<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::TyVar(ctx.fresh_ty_var()))
    }
}

/// A value that has a type.
pub trait HasTy<'ctx> {
    /// Returns the type of the value.
    fn ty(&self) -> Ty<'ctx>;
}

impl<'ctx> HasTy<'ctx> for Ty<'ctx> {
    fn ty(&self) -> Ty<'ctx> {
        *self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Typed<'ctx, T> {
    pub value: T,
    pub ty: Ty<'ctx>,
}

impl<'ctx, T> Typed<'ctx, T> {
    pub fn new(value: T, ty: Ty<'ctx>) -> Self {
        Self { value, ty }
    }
}

impl<'ctx, T> HasTy<'ctx> for Typed<'ctx, T> {
    fn ty(&self) -> Ty<'ctx> {
        self.ty
    }
}

impl<'ctx, T: Default> Typed<'ctx, T> {
    /// Creates a new `Typed` with the default value of `T`.
    pub fn take(&mut self) -> Self {
        Self {
            value: std::mem::take(self),
            ty: self.ty,
        }
    }
}

impl<'ctx, T> std::ops::Deref for Typed<'ctx, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'ctx, T> std::ops::DerefMut for Typed<'ctx, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

into_diag_arg_using_display!(Ty<'_>, TyVarId);
