use data_structure::{arena::Box, interning::Interned};

pub use ir_typed_ast::{BinOp, DisambiguatedIdent, LitKind, Typed, UnOp};

pub type Ident<'ctx> = Interned<'ctx, Typed<'ctx, DisambiguatedIdent<'ctx>>>;

pub type Expr<'ctx> = Box<'ctx, Typed<'ctx, ExprKind<'ctx>>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Ident<'ctx>),
    Binary(BinOp, Ident<'ctx>, Ident<'ctx>),
    If(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinding<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    App(Ident<'ctx>, Vec<Ident<'ctx>>),
    Tuple(Vec<Ident<'ctx>>),
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
    pub place: Pattern<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub value: Expr<'ctx>,
}

impl<'ctx> LetBinding<'ctx> {
    pub fn let_var(place: Ident<'ctx>, value: Expr<'ctx>) -> Self {
        Self {
            place: Pattern::Var(place),
            args: Vec::new(),
            value,
        }
    }
    pub fn let_discard(value: Expr<'ctx>) -> Self {
        debug_assert!(value.ty.is_unit());
        Self {
            place: Pattern::Unit,
            args: Vec::new(),
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Unit,
    Var(Ident<'ctx>),
    Tuple(Vec<Ident<'ctx>>),
}

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
