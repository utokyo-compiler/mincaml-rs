use crate::syntax::{BinOp, Expr, ExprKind, Ident, LetBinding, LitKind, Pattern, UnOp};
use data_structure::FxHashSet;
use ir_typed_ast::Ty;

macro_rules! declare_visitor {
    ($name:ident, $($mutability:ident)?) => {
        pub trait $name<'ctx> {
            /// Visit a type immutably.
            ///
            /// Modifying the type here is not allowed.
            /// Instead, you can alter the type inference of AST or
            /// do another type inference at arbitrary phase.
            /// You can also patch the tree directly.
            ///
            /// N.B. Removing `ty` from `Ident` can make this mutable.
            fn visit_ty(&mut self, _ty: &Ty<'ctx>) {}

            fn visit_expr(&mut self, expr: & $($mutability)? Expr<'ctx>) {
                self.super_expr(expr);
            }

            fn super_expr(&mut self, expr: & $($mutability)? Expr<'ctx>) {
                self.visit_ty(&expr.ty);
                self.visit_expr_kind(& $($mutability)? expr.value);
            }

            fn visit_expr_kind(&mut self, expr: & $($mutability)? ExprKind<'ctx>) {
                self.super_expr_kind(expr);
            }

            fn super_expr_kind(&mut self, expr: & $($mutability)? ExprKind<'ctx>) {
                match expr {
                    ExprKind::Const(lit) => {
                        self.visit_lit(lit);
                    }
                    ExprKind::Unary(un_op, e) => {
                        self.visit_un_op(*un_op, e);
                    }
                    ExprKind::Binary(bin_op, e1, e2) => {
                        self.visit_bin_op(*bin_op, e1, e2);
                    }
                    ExprKind::If(e1, e2, e3) => {
                        self.visit_ident(e1);
                        self.visit_expr(e2);
                        self.visit_expr(e3);
                    }
                    ExprKind::Let(binding, e) => {
                        self.visit_binding(binding);
                        self.visit_expr(e);
                    }
                    ExprKind::Var(ident) => self.visit_ident(ident),
                    ExprKind::App(e, es) => {
                        self.visit_app(e, es);
                    }
                    ExprKind::Tuple(es) => {
                        for e in es {
                            self.visit_ident(e);
                        }
                    }
                    ExprKind::ArrayMake(e1, e2) => {
                        self.visit_ident(e1);
                        self.visit_ident(e2);
                    }
                    ExprKind::Get(e1, e2) => {
                        self.visit_ident(e1);
                        self.visit_ident(e2);
                    }
                    ExprKind::Set(e1, e2, e3) => {
                        self.visit_ident(e1);
                        self.visit_ident(e2);
                        self.visit_ident(e3);
                    }
                }
            }

            fn visit_lit(&mut self, _lit: & $($mutability)? LitKind) {}

            fn visit_un_op(&mut self, un_op: UnOp, e: & $($mutability)? Ident<'ctx>) {
                self.super_un_op(un_op, e);
            }

            fn super_un_op(&mut self, _un_op: UnOp, e: & $($mutability)? Ident<'ctx>) {
                self.visit_ident(e);
            }

            fn visit_bin_op(&mut self, bin_op: BinOp, e1: & $($mutability)? Ident<'ctx>, e2: & $($mutability)? Ident<'ctx>) {
                self.super_bin_op(bin_op, e1, e2);
            }

            fn super_bin_op(&mut self, _bin_op: BinOp, e1: & $($mutability)? Ident<'ctx>, e2: & $($mutability)? Ident<'ctx>) {
                self.visit_ident(e1);
                self.visit_ident(e2);
            }

            fn visit_ident(&mut self, ident: & $($mutability)? Ident<'ctx>) {
                self.super_ident(ident);
            }

            fn super_ident(&mut self, ident: & $($mutability)? Ident<'ctx>) {
                self.visit_ty(&ident.ty);
            }

            fn visit_binding(&mut self, binding: & $($mutability)? LetBinding<'ctx>) {
                self.super_binding(binding);
            }

            fn super_binding(&mut self, binding: & $($mutability)? LetBinding<'ctx>) {
                let LetBinding { place, args, value } = binding;
                self.visit_pattern(place);
                for arg in args {
                    self.visit_ident(arg);
                }
                self.visit_expr(value);
            }

            fn visit_pattern(&mut self, pattern: & $($mutability)? Pattern<'ctx>) {
                self.super_pattern(pattern);
            }

            fn super_pattern(&mut self, pattern: & $($mutability)? Pattern<'ctx>) {
                match pattern {
                    Pattern::Unit => (),
                    Pattern::Var(ident) => self.visit_ident(ident),
                    Pattern::Tuple(idents) => {
                        for ident in idents {
                            self.visit_ident(ident);
                        }
                    }
                }
            }

            fn visit_app(&mut self, e: & $($mutability)? Ident<'ctx>, es: & $($mutability)? Vec<Ident<'ctx>>) {
                self.super_app(e, es);
            }

            fn super_app(&mut self, e: & $($mutability)? Ident<'ctx>, es: & $($mutability)? Vec<Ident<'ctx>>) {
                self.visit_ident(e);
                for e in es {
                    self.visit_ident(e);
                }
            }
        }
    };
}

declare_visitor!(Visitor,);
declare_visitor!(MutVisitor, mut);

pub trait FvVisitor<'ctx> {
    fn visit_fv(&mut self, ident: Ident<'ctx>);
}

/// A reusable fragment of free-variable visitor.
pub struct FvVisitorHelper<'ctx, F: FvVisitor<'ctx>> {
    fv_visitor: F,
    bound_idents: FxHashSet<Ident<'ctx>>,
}

impl<'ctx, F> FvVisitorHelper<'ctx, F>
where
    F: FvVisitor<'ctx>,
{
    pub fn new(fv_visitor: F) -> Self {
        Self {
            fv_visitor,
            bound_idents: FxHashSet::default(),
        }
    }

    pub fn super_ident(&mut self, ident: &Ident<'ctx>) {
        if !self.bound_idents.contains(ident) {
            self.fv_visitor.visit_fv(*ident);
        }
    }

    pub fn super_binding_args(&mut self, binding_args: &[Ident<'ctx>]) {
        for arg in binding_args {
            self.bound_idents.insert(*arg);
        }
    }

    pub fn super_pattern(&mut self, pattern: &Pattern<'ctx>) {
        match pattern {
            Pattern::Unit => (),
            Pattern::Var(ident) => {
                self.bound_idents.insert(*ident);
            }
            Pattern::Tuple(idents) => {
                for ident in idents {
                    self.bound_idents.insert(*ident);
                }
            }
        }
    }
}
