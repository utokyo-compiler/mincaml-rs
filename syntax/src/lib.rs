use std::fmt::Display;

use data_structure::interning::Interned;
use sourcemap::Spanned;

pub type Ident<'ctx> = Interned<'ctx, str>;

pub type Expr<'ctx> = Spanned<Interned<'ctx, ExprKind<'ctx>>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Expr<'ctx>),
    Binary(BinOp, Expr<'ctx>, Expr<'ctx>),
    If(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetKind<'ctx>),
    Then(Expr<'ctx>, Expr<'ctx>),
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
        // .( ,
        Expr<'ctx>,
        // ) <- ,
        Expr<'ctx>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunDef<'ctx> {
    pub name: Ident<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub body: Expr<'ctx>,
}

impl<'ctx> FunDef<'ctx> {
    pub fn new(name: Ident<'ctx>, args: Vec<Ident<'ctx>>, body: Expr<'ctx>) -> Self {
        Self { name, args, body }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BBinOpKind {
    Eq,
    Le,
    Ge,
    Ne,
    Lt,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IBinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    BBinOp(BBinOpKind),
    IBinOp(IBinOpKind),
    FBinOp(FBinOpKind),
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

impl Display for LitKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitKind::Unit => write!(f, "()"),
            LitKind::Bool(b) => write!(f, "{b}"),
            LitKind::Int(i) => write!(f, "{i}"),
            LitKind::Float(f_bits) => write!(f, "{}", f32::from_bits(*f_bits)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LetKind<'ctx> {
    LetVar(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    LetRec(FunDef<'ctx>, Expr<'ctx>),
    LetTuple(Vec<Ident<'ctx>>, Expr<'ctx>, Expr<'ctx>),
}
