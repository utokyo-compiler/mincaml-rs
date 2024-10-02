mod prettify;

use data_structure::interning::Interned;
use sourcemap::Spanned;

pub type Ident<'ctx> = Interned<'ctx, str>;

pub type Expr<'ctx> = &'ctx Spanned<ExprKind<'ctx>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Expr<'ctx>),
    Binary(BinOp, Expr<'ctx>, Expr<'ctx>),
    If(
        // if
        Expr<'ctx>,
        // then
        Expr<'ctx>,
        // else
        Expr<'ctx>,
    ),
    Let(LetBinder<'ctx>, Expr<'ctx>),
    Then(
        Expr<'ctx>,
        // ;
        Expr<'ctx>,
    ),
    Var(Ident<'ctx>),
    App(Expr<'ctx>, Vec<Expr<'ctx>>),
    Tuple(Vec<Expr<'ctx>>),
    ArrayMake(Expr<'ctx>, Expr<'ctx>),
    Get(
        Expr<'ctx>,
        // .(
        Expr<'ctx>,
        // ) ,
    ),
    Set(
        Expr<'ctx>,
        // <- ,
        Expr<'ctx>,
    ),
}

impl<'ctx> ExprKind<'ctx> {
    /// Returns the kind of the expression. This is useful for dereferencing.
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BooleanBinOpKind {
    Eq,
    Le,
    Ge,
    Ne,
    Lt,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntBinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatBinOpKind {
    FAdd,
    FSub,
    FMul,
    FDiv,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Boolean(BooleanBinOpKind),
    Int(IntBinOpKind),
    Float(FloatBinOpKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Not,
    Neg,
    FNeg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LitKind {
    Unit,
    Bool(bool),
    Int(i32),
    Float(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetBinder<'ctx> {
    pattern: Pattern<'ctx>,
    args: Vec<Spanned<Ident<'ctx>>>,
    value: Expr<'ctx>,
}

impl<'ctx> LetBinder<'ctx> {
    pub fn let_var(pattern: Spanned<Ident<'ctx>>, value: Expr<'ctx>) -> Self {
        Self {
            pattern: Pattern::Var(pattern),
            args: Vec::new(),
            value,
        }
    }

    pub fn let_rec(
        name: Spanned<Ident<'ctx>>,
        args: Vec<Spanned<Ident<'ctx>>>,
        value: Expr<'ctx>,
    ) -> Self {
        Self {
            pattern: Pattern::Var(name),
            args,
            value,
        }
    }

    pub fn let_tuple(pattern: Vec<Spanned<Ident<'ctx>>>, value: Expr<'ctx>) -> Self {
        Self {
            pattern: Pattern::Tuple(pattern),
            args: Vec::new(),
            value,
        }
    }

    pub fn pattern(&self) -> &Pattern<'ctx> {
        &self.pattern
    }

    pub fn has_args(&self) -> bool {
        !self.args.is_empty()
    }

    pub fn args(&'ctx self) -> impl Iterator<Item = Spanned<Ident<'ctx>>> {
        self.args.iter().copied()
    }

    pub fn value(&self) -> Expr<'ctx> {
        self.value
    }

    pub fn len_args(&self) -> usize {
        self.args.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Var(Spanned<Ident<'ctx>>),
    Tuple(Vec<Spanned<Ident<'ctx>>>),
}
