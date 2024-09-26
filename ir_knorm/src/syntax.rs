use data_structure::{
    arena::Box,
    index::{vec::IndexVec, Indexable},
    interning::Interned,
};

pub use ir_typed_ast::{
    ArgIndex, BinOp, DisambiguatedIdent, LitKind, TupleIndex, Ty, Typed, TypedIdent, UnOp,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Ident<'ctx>(Interned<'ctx, Typed<'ctx, DisambiguatedIdent<'ctx>>>);

impl<'ctx> Ident<'ctx> {
    pub fn new(interned: Interned<'ctx, Typed<'ctx, DisambiguatedIdent<'ctx>>>) -> Self {
        Self(interned)
    }
}

impl<'ctx> std::ops::Deref for Ident<'ctx> {
    type Target = Interned<'ctx, Typed<'ctx, DisambiguatedIdent<'ctx>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'ctx> std::ops::DerefMut for Ident<'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
pub type TypedExprKind<'ctx> = Typed<'ctx, ExprKind<'ctx>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Ident<'ctx>),
    Binary(BinOp, Ident<'ctx>, Ident<'ctx>),
    If(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinding<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    App(Ident<'ctx>, IndexVec<ArgIndex, Ident<'ctx>>),
    Tuple(IndexVec<TupleIndex, Ident<'ctx>>),
    ArrayMake(Ident<'ctx>, Ident<'ctx>),
    Get(Ident<'ctx>, Ident<'ctx>),
    Set(Ident<'ctx>, Ident<'ctx>, Ident<'ctx>),
}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }

    pub fn as_var(&self) -> Option<Ident<'ctx>> {
        if let Self::Var(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

impl<'ctx> ExprKind<'ctx> {
    pub fn name(&self) -> &'static str {
        match self {
            ExprKind::Const(lit) => lit.discriminant_name(),
            ExprKind::Unary(un_op, ..) => un_op.name(),
            ExprKind::Binary(bin_op, ..) => bin_op.name(),
            ExprKind::If(..) => "if",
            ExprKind::Let(..) => "let",
            ExprKind::Var(..) => "var",
            ExprKind::App(..) => "app",
            ExprKind::Tuple(..) => "tuple",
            ExprKind::ArrayMake(..) => "Array.make",
            ExprKind::Get(..) => "get",
            ExprKind::Set(..) => "set",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<'ctx> {
    pub pattern: Pattern<'ctx>,
    pub args: IndexVec<ArgIndex, Ident<'ctx>>,
    pub value: Expr<'ctx>,
}

impl<'ctx> LetBinding<'ctx> {
    pub fn let_var(pattern: Ident<'ctx>, value: Expr<'ctx>) -> Self {
        Self {
            pattern: Pattern::Var(pattern),
            args: IndexVec::new(),
            value,
        }
    }
    pub fn let_discard(value: Expr<'ctx>) -> Self {
        debug_assert!(value.ty.is_unit());
        Self {
            pattern: Pattern::Unit,
            args: IndexVec::new(),
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Unit,
    Var(Ident<'ctx>),
    Tuple(IndexVec<TupleIndex, Ident<'ctx>>),
}

impl Indexable<ArgIndex> for Ident<'_> {}
impl Indexable<TupleIndex> for Ident<'_> {}

impl<'ctx> Pattern<'ctx> {
    pub fn as_var(&self) -> Option<Ident<'ctx>> {
        if let Self::Var(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_tuple(&self) -> Option<&Vec<Ident<'ctx>>> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
