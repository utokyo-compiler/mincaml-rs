use std::fmt::Display;

use sourcemap::Spanned;

pub type Ident<'input> = &'input str;

pub type Expr<'a> = Spanned<ExprKind<'a>>;

#[derive(Debug, Clone)]
pub enum ExprKind<'input> {
    Lit(LitKind),
    Unary(UnOp, Box<Expr<'input>>),
    Binary(BinOp, Box<Expr<'input>>, Box<Expr<'input>>),
    If(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),
    Let(LetKind<'input>),
    Then(Box<Expr<'input>>, Box<Expr<'input>>),
    Var(Ident<'input>),
    App(Box<Expr<'input>>, Vec<Expr<'input>>),
    Tuple(Vec<Expr<'input>>),
    ArrayMake(Box<Expr<'input>>, Box<Expr<'input>>),
    Get(Box<Expr<'input>>, Box<Expr<'input>>),
    Set(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),
}

#[derive(Debug, Clone)]
pub struct FunDef<'a> {
    pub name: Ident<'a>,
    pub args: Vec<Ident<'a>>,
    pub body: Box<Expr<'a>>,
}

impl<'a> FunDef<'a> {
    pub fn new(name: Ident<'a>, args: Vec<Ident<'a>>, body: Box<Expr<'a>>) -> Self {
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
pub enum LetKind<'a> {
    LetVar(Ident<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    LetRec(FunDef<'a>, Box<Expr<'a>>),
    LetTuple(Vec<Ident<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
}
