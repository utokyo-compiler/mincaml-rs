use data_structure::arena::Box;

pub use ir_knorm::{BinOp, DisambiguatedIdent, Ident, LitKind, Pattern, Ty, Typed, UnOp};

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
pub type TypedExprKind<'ctx> = Typed<'ctx, ExprKind<'ctx>>;

pub struct Program<'ctx> {
    pub functions: Vec<Function<'ctx>>,
    pub main: Expr<'ctx>,
}

/// A function definition. Functions may capture variables from the environment.
pub struct Function<'ctx> {
    pub name: DisambiguatedIdent<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub args_via_closure: Vec<Ident<'ctx>>,
    pub body: Expr<'ctx>,
}

impl<'ctx> Function<'ctx> {
    pub fn ret_ty(&self) -> Ty<'ctx> {
        self.body.ty
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Ident<'ctx>),
    Binary(BinOp, Ident<'ctx>, Ident<'ctx>),
    If(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinding<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    /// Create a closure which captures the given arguments.
    ClosureMake(Closure<'ctx>),
    App(ApplyKind, Ident<'ctx>, Vec<Ident<'ctx>>),
    Tuple(Vec<Ident<'ctx>>),
    ArrayMake(Ident<'ctx>, Ident<'ctx>),
    Get(Ident<'ctx>, Ident<'ctx>),
    Set(Ident<'ctx>, Ident<'ctx>, Ident<'ctx>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Closure<'ctx> {
    pub fn_name: Ident<'ctx>,
    pub captured_args: Vec<Ident<'ctx>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ApplyKind {
    Direct,
    Closure,
}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<'ctx> {
    pub place: Pattern<'ctx>,
    pub value: Expr<'ctx>,
}
