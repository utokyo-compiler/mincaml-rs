use std::fmt::Debug;

use data_structure::interning::Interned;
use sourcemap::Spanned;
use ty::Typed;

#[derive(Debug, Clone)]
pub struct DisambiguatedIdent<'ctx> {
    pub name: syntax::Ident<'ctx>,
    disambiguator: u32,
}

pub type Ident<'ctx> = Typed<'ctx, DisambiguatedIdent<'ctx>>;

pub type Expr<'ctx> = Typed<'ctx, Spanned<Interned<'ctx, ExprKind<'ctx>>>>;

#[derive(Debug, Clone)]
pub enum ExprKind<'ctx> {
    Const(syntax::LitKind),
    Unary(syntax::UnOp, Expr<'ctx>),
    Binary(syntax::BinOp, Expr<'ctx>, Expr<'ctx>),
    If(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetKind<'ctx>),
    Then(Expr<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    App(Expr<'ctx>, Vec<Expr<'ctx>>),
    Tuple(Vec<Expr<'ctx>>),
    ArrayMake(Expr<'ctx>, Expr<'ctx>),
    Get(Expr<'ctx>, Expr<'ctx>),
    Set(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
}

#[derive(Debug, Clone)]
pub struct FunDef<'ctx> {
    pub name: Ident<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub body: Expr<'ctx>,
}

#[derive(Debug, Clone)]
pub enum LetKind<'ctx> {
    LetVar(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    LetRec(FunDef<'ctx>, Expr<'ctx>),
    LetTuple(Vec<Ident<'ctx>>, Expr<'ctx>, Expr<'ctx>),
}
