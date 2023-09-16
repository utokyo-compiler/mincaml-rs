use std::fmt::Display;

use sourcemap::Spanned;

pub type Ident<'input> = &'input str;

pub type Expr<'a, 'b> = Spanned<ExprKind<'a, 'b>>;

#[derive(Debug, Clone)]
pub enum ExprKind<'input, 'b> {
    Lit(LitKind),
    Unary(UnOp, &'b Expr<'input, 'b>),
    Binary(BinOp, &'b Expr<'input, 'b>, &'b Expr<'input, 'b>),
    If(
        &'b Expr<'input, 'b>,
        &'b Expr<'input, 'b>,
        &'b Expr<'input, 'b>,
    ),
    Let(LetKind<'input, 'b>),
    Then(&'b Expr<'input, 'b>, &'b Expr<'input, 'b>),
    Var(Ident<'input>),
    App(&'b Expr<'input, 'b>, Vec<Expr<'input, 'b>>),
    Tuple(Vec<Expr<'input, 'b>>),
    ArrayMake(&'b Expr<'input, 'b>, &'b Expr<'input, 'b>),
    Get(&'b Expr<'input, 'b>, &'b Expr<'input, 'b>),
    Set(
        &'b Expr<'input, 'b>,
        &'b Expr<'input, 'b>,
        &'b Expr<'input, 'b>,
    ),
}

#[derive(Debug, Clone)]
pub struct FunDef<'a, 'b> {
    pub name: Ident<'a>,
    pub args: Vec<Ident<'a>>,
    pub body: &'b Expr<'a, 'b>,
}

impl<'a, 'b> FunDef<'a, 'b> {
    pub fn new(name: Ident<'a>, args: Vec<Ident<'a>>, body: &'b Expr<'a, 'b>) -> Self {
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
pub enum LetKind<'a, 'b> {
    LetVar(Ident<'a>, &'b Expr<'a, 'b>, &'b Expr<'a, 'b>),
    LetRec(FunDef<'a, 'b>, &'b Expr<'a, 'b>),
    LetTuple(Vec<Ident<'a>>, &'b Expr<'a, 'b>, &'b Expr<'a, 'b>),
}
