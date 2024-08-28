use std::fmt::Display;

use sourcemap::Spanned;

pub type Ident<'input> = &'input str;

pub type Expr<'input, 'arena> = Spanned<ExprKind<'input, 'arena>>;

#[derive(Debug, Clone)]
pub enum ExprKind<'input, 'arena> {
    Lit(LitKind),
    Unary(UnOp, &'arena Expr<'input, 'arena>),
    Binary(
        BinOp,
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
    ),
    If(
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
    ),
    Let(LetKind<'input, 'arena>),
    Then(&'arena Expr<'input, 'arena>, &'arena Expr<'input, 'arena>),
    Var(Ident<'input>),
    App(&'arena Expr<'input, 'arena>, Vec<Expr<'input, 'arena>>),
    Tuple(Vec<Expr<'input, 'arena>>),
    ArrayMake(&'arena Expr<'input, 'arena>, &'arena Expr<'input, 'arena>),
    Get(&'arena Expr<'input, 'arena>, &'arena Expr<'input, 'arena>),
    Set(
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
    ),
}

#[derive(Debug, Clone)]
pub struct FunDef<'input, 'arena> {
    pub name: Ident<'input>,
    pub args: Vec<Ident<'input>>,
    pub body: &'arena Expr<'input, 'arena>,
}

impl<'input, 'arena> FunDef<'input, 'arena> {
    pub fn new(
        name: Ident<'input>,
        args: Vec<Ident<'input>>,
        body: &'arena Expr<'input, 'arena>,
    ) -> Self {
        Self { name, args, body }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BBinOpKind {
    Eq,
    Le,
    Ge,
    Ne,
    Lt,
    Gt,
}

#[derive(Debug, Clone, Copy)]
pub enum IBinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub enum FBinOpKind {
    FAdd,
    FSub,
    FMul,
    FDiv,
}

#[derive(Debug, Clone, Copy)]
pub enum FUnOpKind {
    Fneg,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    BBinOp(BBinOpKind),
    IBinOp(IBinOpKind),
    FBinOp(FBinOpKind),
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    Neg,
    FNeg,
}

#[derive(Debug, Clone, Copy)]
pub enum LitKind {
    Unit,
    Bool(bool),
    Int(i32),
    Float(f32),
}

impl Display for LitKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitKind::Unit => write!(f, "()"),
            LitKind::Bool(b) => write!(f, "{b}"),
            LitKind::Int(i) => write!(f, "{i}"),
            LitKind::Float(fl) => write!(f, "{fl}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LetKind<'input, 'arena> {
    LetVar(
        Ident<'input>,
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
    ),
    LetRec(FunDef<'input, 'arena>, &'arena Expr<'input, 'arena>),
    LetTuple(
        Vec<Ident<'input>>,
        &'arena Expr<'input, 'arena>,
        &'arena Expr<'input, 'arena>,
    ),
}
