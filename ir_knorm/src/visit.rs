use crate::syntax::{BinOp, Expr, ExprKind, Ident, LetBinder, LitKind, Pattern, UnOp};
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
                match & $($mutability)? expr.value {
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
                    ExprKind::Let(binder, e) => {
                        self.visit_binder(binder);
                        self.visit_expr(e);
                    }
                    ExprKind::Var(ident) => self.visit_ident(ident),
                    ExprKind::App(e, es) => {
                        self.visit_ident(e);
                        for e in es {
                            self.visit_ident(e);
                        }
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

            fn visit_binder(&mut self, binder: & $($mutability)? LetBinder<'ctx>) {
                self.super_binder(binder);
            }

            fn super_binder(&mut self, binder: & $($mutability)? LetBinder<'ctx>) {
                let LetBinder { place, args, value } = binder;
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
        }
    };
}

declare_visitor!(Visitor,);
declare_visitor!(MutVisitor, mut);
