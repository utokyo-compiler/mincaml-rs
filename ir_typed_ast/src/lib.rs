use std::{cell::RefCell, fmt::Debug};

use data_structure::interning::Interned;
use sourcemap::Spanned;
use ty::{TyKind, Typed};

pub type Ident<'input, 'ctx> = Typed<'ctx, syntax::Ident<'input>>;

pub type Expr<'input, 'ctx> = Typed<'ctx, Spanned<&'ctx ExprKind<'input, 'ctx>>>;

#[derive(Debug, Clone)]
pub enum ExprKind<'input, 'ctx> {
    Const(syntax::LitKind),
    Unary(syntax::UnOp, Expr<'input, 'ctx>),
    Binary(syntax::BinOp, Expr<'input, 'ctx>, Expr<'input, 'ctx>),
    If(Expr<'input, 'ctx>, Expr<'input, 'ctx>, Expr<'input, 'ctx>),
    Let(LetKind<'input, 'ctx>),
    Then(Expr<'input, 'ctx>, Expr<'input, 'ctx>),
    Var(Ident<'input, 'ctx>),
    App(Expr<'input, 'ctx>, Vec<Expr<'input, 'ctx>>),
    Tuple(Vec<Expr<'input, 'ctx>>),
    ArrayMake(Expr<'input, 'ctx>, Expr<'input, 'ctx>),
    Get(Expr<'input, 'ctx>, Expr<'input, 'ctx>),
    Set(Expr<'input, 'ctx>, Expr<'input, 'ctx>, Expr<'input, 'ctx>),
}

#[derive(Debug, Clone)]
pub struct FunDef<'input, 'ctx> {
    pub name: Ident<'input, 'ctx>,
    pub args: Vec<Ident<'input, 'ctx>>,
    pub body: Expr<'input, 'ctx>,
}

#[derive(Debug, Clone)]
pub enum LetKind<'input, 'ctx> {
    LetVar(Ident<'input, 'ctx>, Expr<'input, 'ctx>, Expr<'input, 'ctx>),
    LetRec(FunDef<'input, 'ctx>, Expr<'input, 'ctx>),
    LetTuple(
        Vec<Ident<'input, 'ctx>>,
        Expr<'input, 'ctx>,
        Expr<'input, 'ctx>,
    ),
}
