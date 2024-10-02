use ir_knorm::{Expr, ExprKind, Ident, Visitor, MutVisitor, ArgIndex};
use middleware::FxHashMap;
use data_structure::index::vec::IndexVec;

use crate::{alpha_rename::AlphaRename, KnormPass};

pub struct Inlining;

fn calculate_size<'ctx>(expr: &Expr<'ctx>) -> usize {
    struct SizeVisitor {
        pub size: usize,
    }

    impl<'ctx> Visitor<'ctx> for SizeVisitor {
        fn visit_expr(&mut self, expr: &Expr<'ctx>) {
            self.size += 1;
            self.super_expr(expr);
        }
    }

    let mut visitor = SizeVisitor { size: 0 };
    visitor.visit_expr(expr);

    visitor.size
}

impl<'ctx> KnormPass<'ctx> for Inlining {
    fn run_pass(&mut self, ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct Hoge<'ctx> {
            ctx: &'ctx middleware::GlobalContext<'ctx>,
            env: FxHashMap<Ident<'ctx>, (IndexVec<ArgIndex, Ident<'ctx>>, Expr<'ctx>)>,
        }

        impl<'ctx> MutVisitor<'ctx> for Hoge<'ctx> {
            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                match &mut expr.value {
                    // function definition
                    ExprKind::Let(binding, _) if binding.is_function() => {
                        if let Some(f_var) = binding.pattern.as_var() {
                            if calculate_size(&binding.value) < self.ctx.compiler_option().inline_size_limit {
                                self.env.insert(f_var, (binding.args.clone(), binding.value.clone()));
                            }
                        }
                        else {
                            unreachable!()
                        }
                        self.super_expr(expr);
                    }
                    ExprKind::App(f_var, args) => {
                        if let Some((formal_args, body)) = self.env.get(f_var) {
                            let mut initial_env = FxHashMap::default();
                            for (x, y) in args.iter().zip(formal_args) {
                                initial_env.insert(*y, *x);
                            }
                            let mut body = body.clone();
                            AlphaRename::new(initial_env).run_pass(self.ctx, &mut body);

                            *expr = body;
                        }
                    },
                    _ => {
                        self.super_expr(expr);
                    }
                }
            }
        }

        Hoge {
            ctx,
            env: FxHashMap::default(),
        }
        .visit_expr(expr);
    }
}
