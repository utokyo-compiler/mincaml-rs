use data_structure::{
    arena::Box,
    index::{vec::IndexVec, Indexable},
    interning::Interned,
};

pub use ir_typed_ast::{
    ArgIndex, BinOp, DisambiguatedIdent, FloatBinOpKind, IntBinOpKind, LitKind, RelationBinOpKind,
    TupleIndex, Ty, TyKind, Typed, TypedIdent, UnOp,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Ident<'ctx>),
    Binary(BinOp, Ident<'ctx>, Ident<'ctx>),
    If(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetExpr<'ctx>),
    Var(Ident<'ctx>),
    App(Ident<'ctx>, IndexVec<ArgIndex, Ident<'ctx>>),
    Tuple(IndexVec<TupleIndex, Ident<'ctx>>),
    ArrayMake(Ident<'ctx>, Ident<'ctx>),
    Get(Ident<'ctx>, Ident<'ctx>),
    Set(Ident<'ctx>, Ident<'ctx>, Ident<'ctx>),

    #[default]
    /// Invalid expression.
    ///
    /// Useful for discarding values, should not occur anywhere else. This variant will be
    /// introduced mostly in `Typed::take()`.
    Invalid,
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

    /// Returns `true` if the expr kind is [`Let`].
    ///
    /// [`Let`]: ExprKind::Let
    #[must_use]
    pub fn is_let(&self) -> bool {
        matches!(self, Self::Let(..))
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

            ExprKind::Invalid => unreachable!("invalid expression"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LetExpr<'ctx> {
    /// (`let x = e`)`in e`
    pub binding: LetBinding<'ctx>,

    /// `let x = e in`(`e`)
    body: Option<Expr<'ctx>>,
}

impl<'ctx> LetExpr<'ctx> {
    pub fn new(binding: LetBinding<'ctx>, body: Expr<'ctx>) -> Self {
        Self {
            binding,
            body: Some(body),
        }
    }

    pub fn body(&self) -> &Expr<'ctx> {
        self.body.as_ref().unwrap()
    }

    pub fn body_mut(&mut self) -> &mut Expr<'ctx> {
        self.body.as_mut().unwrap()
    }

    pub fn take_body(&mut self) -> Option<Expr<'ctx>> {
        self.body.take()
    }

    pub fn set_body(&mut self, body: Expr<'ctx>) {
        self.body = Some(body)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<'ctx> {
    pub pattern: Pattern<'ctx>,
    pub args: IndexVec<ArgIndex, Ident<'ctx>>,
    bindee: Option<Expr<'ctx>>,
}

impl<'ctx> LetBinding<'ctx> {
    pub fn new(
        pattern: Pattern<'ctx>,
        args: IndexVec<ArgIndex, Ident<'ctx>>,
        value: Expr<'ctx>,
    ) -> Self {
        Self {
            pattern,
            args,
            bindee: Some(value),
        }
    }

    pub fn let_var(pattern: Ident<'ctx>, value: Expr<'ctx>) -> Self {
        Self {
            pattern: Pattern::Var(pattern),
            args: IndexVec::new(),
            bindee: Some(value),
        }
    }
    pub fn let_discard(value: Expr<'ctx>) -> Self {
        debug_assert!(value.ty.is_unit());
        Self {
            pattern: Pattern::Unit,
            args: IndexVec::new(),
            bindee: Some(value),
        }
    }
    pub fn is_function(&self) -> bool {
        !self.args.is_empty()
    }

    pub fn bindee(&self) -> &Expr<'ctx> {
        self.bindee.as_ref().unwrap()
    }

    pub fn bindee_mut(&mut self) -> &mut Expr<'ctx> {
        self.bindee.as_mut().unwrap()
    }

    pub fn take_bindee(&mut self) -> Option<Expr<'ctx>> {
        self.bindee.take()
    }

    pub fn set_bindee(&mut self, value: Expr<'ctx>) {
        self.bindee = Some(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Pattern<'ctx> {
    #[default]
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
