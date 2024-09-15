use std::fmt::Debug;

use data_structure::arena::Box;
use sourcemap::Spanned;
use ty::{Ty, Typed};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DisambiguatedIdent<'ctx> {
    pub name: syntax::Ident<'ctx>,
    origin: IdentOrigin,
    disambiguator: u32,
}

impl<'ctx> DisambiguatedIdent<'ctx> {
    pub fn new_unchecked(
        name: syntax::Ident<'ctx>,
        origin: IdentOrigin,
        disambiguator: u32,
    ) -> Self {
        Self {
            name,
            origin,
            disambiguator,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IdentOrigin {
    UserDefined,
    CompilerGenerated,
}

pub type Ident<'ctx> = Typed<'ctx, DisambiguatedIdent<'ctx>>;

pub type Expr<'ctx> = Typed<'ctx, Box<'ctx, Spanned<ExprKind<'ctx>>>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(syntax::LitKind),
    Unary(syntax::UnOp, Expr<'ctx>),
    Binary(syntax::BinOp, Expr<'ctx>, Expr<'ctx>),
    If(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinder<'ctx>, Expr<'ctx>),
    Then(Expr<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    App(Expr<'ctx>, Vec<Expr<'ctx>>),
    Tuple(Vec<Expr<'ctx>>),
    ArrayMake(Expr<'ctx>, Expr<'ctx>),
    Get(Expr<'ctx>, Expr<'ctx>),
    Set(Expr<'ctx>, Expr<'ctx>, Expr<'ctx>),
}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug)]
pub struct FunDef<'ctx> {
    pub name: Ident<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub body: Expr<'ctx>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinder<'ctx> {
    pub place: Pattern<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub value: Expr<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Var(Ident<'ctx>),
    Tuple(Vec<Ident<'ctx>>),
}

macro_rules! declare_visitor {
    ($name:ident, $($mutability:ident)?) => {
        pub trait $name<'ctx> {
            fn visit_ty(&mut self, ty: & $($mutability)? Ty<'ctx>) {
                self.super_ty(ty);
            }

            fn super_ty(&mut self, _ty: & $($mutability)? Ty<'ctx>) {}

            fn visit_expr(&mut self, expr: & $($mutability)? Expr<'ctx>) {
                self.super_expr(expr);
            }

            fn super_expr(&mut self, expr: & $($mutability)? Expr<'ctx>) {
                self.visit_ty(& $($mutability)? expr.ty);
                match & $($mutability)? expr.value.node {
                    ExprKind::Const(lit) => {
                        self.visit_lit(lit);
                    }
                    ExprKind::Unary(_, e) => self.visit_expr(e),
                    ExprKind::Binary(_, e1, e2) => {
                        self.visit_expr(e1);
                        self.visit_expr(e2);
                    }
                    ExprKind::If(e1, e2, e3) => {
                        self.visit_expr(e1);
                        self.visit_expr(e2);
                        self.visit_expr(e3);
                    }
                    ExprKind::Let(binder, e) => {
                        self.visit_pattern(& $($mutability)? binder.place);
                        for arg in & $($mutability)? binder.args {
                            self.visit_ident(arg);
                        }
                        self.visit_expr(e);
                    }
                    ExprKind::Then(e1, e2) => {
                        self.visit_expr(e1);
                        self.visit_expr(e2);
                    }
                    ExprKind::Var(ident) => self.visit_ident(ident),
                    ExprKind::App(e, es) => {
                        self.visit_expr(e);
                        for e in es {
                            self.visit_expr(e);
                        }
                    }
                    ExprKind::Tuple(es) => {
                        for e in es {
                            self.visit_expr(e);
                        }
                    }
                    ExprKind::ArrayMake(e1, e2) => {
                        self.visit_expr(e1);
                        self.visit_expr(e2);
                    }
                    ExprKind::Get(e1, e2) => {
                        self.visit_expr(e1);
                        self.visit_expr(e2);
                    }
                    ExprKind::Set(e1, e2, e3) => {
                        self.visit_expr(e1);
                        self.visit_expr(e2);
                        self.visit_expr(e3);
                    }
                }
            }

            fn visit_lit(&mut self, lit: & $($mutability)? syntax::LitKind) {
                self.super_lit(lit);
            }

            fn super_lit(&mut self, _lit: & $($mutability)? syntax::LitKind) {}

            fn visit_ident(&mut self, ident: & $($mutability)? Ident<'ctx>) {
                self.super_ident(ident);
            }

            fn super_ident(&mut self, ident: & $($mutability)? Ident<'ctx>) {
                self.visit_ty(& $($mutability)? ident.ty);
            }

            fn visit_pattern(&mut self, pattern: & $($mutability)? Pattern<'ctx>) {
                self.super_pattern(pattern);
            }

            fn super_pattern(&mut self, pattern: & $($mutability)? Pattern<'ctx>) {
                match pattern {
                    Pattern::Var(ident) => self.visit_ident(ident),
                    Pattern::Tuple(idents) => {
                        for ident in idents {
                            self.visit_ident(ident);
                        }
                    }
                }
            }
        }
    };
}

declare_visitor!(Visitor,);
declare_visitor!(MutVisitor, mut);
