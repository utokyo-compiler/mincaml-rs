use crate::{ApplyKind, Expr, ExprKind, FunctionDef, FunctionInstance, Ident, Program};

use data_structure::interning::Interned;

macro_rules! overload_mut {
    ($receiver:ident, [body],) => {
        $receiver.body()
    };
    ($receiver:ident, [body], mut) => {
        $receiver.body_mut()
    };
}

macro_rules! declare_visitor {
    ($name:ident, $($mutability:ident)?) => {
        pub trait $name<'ctx> {
            fn visit_program(&mut self, program: & $($mutability)? Program<'ctx>) {
                self.super_program(program);
            }

            fn super_program(&mut self, program: & $($mutability)? Program<'ctx>) {
                for function in & $($mutability)? program.functions {
                    self.visit_function_def(function);
                }
            }

            fn visit_function_def(&mut self, function: & $($mutability)? FunctionDef<'ctx>) {
                self.super_function_def(function);
            }

            fn super_function_def(&mut self, function: & $($mutability)? FunctionDef<'ctx>) {
                self.visit_expr(overload_mut!(function, [body], $($mutability)?));
            }

            fn visit_expr(&mut self, expr: & $($mutability)? Expr<'ctx>) {
                self.super_expr(expr);
            }

            fn super_expr(&mut self, expr: & $($mutability)? Expr<'ctx>) {
                match & $($mutability)? expr.value {
                    ExprKind::Const(_) => (),
                    ExprKind::Unary(_, ident) => {
                        self.visit_ident(ident);
                    }
                    ExprKind::Binary(_, lhs, rhs) => {
                        self.visit_ident(lhs);
                        self.visit_ident(rhs);
                    }
                    ExprKind::If(cond, then, els) => {
                        self.visit_ident(cond);
                        self.visit_expr(then);
                        self.visit_expr(els);
                    }
                    ExprKind::Let(_binding, e) => {
                        self.visit_expr(e);
                    }
                    ExprKind::Var(ident) => {
                        self.visit_ident(ident);
                    }
                    ExprKind::ClosureMake(closure) => {
                        self.visit_function_instance(& $($mutability)? closure.function);
                        for arg in & $($mutability)? closure.captured_args {
                            self.visit_ident(arg);
                        }
                    }
                    ExprKind::App(kind, args) => {
                        match kind {
                            ApplyKind::Direct { function } => {
                                self.visit_function_instance(function);
                            }
                            ApplyKind::Closure { ident } => {
                                self.visit_ident(ident);
                            }
                        }
                        for arg in args {
                            self.visit_ident(arg);
                        }
                    }
                    ExprKind::Tuple(idents) => {
                        for ident in idents {
                            self.visit_ident(ident);
                        }
                    }
                    ExprKind::ArrayMake(size, init) => {
                        self.visit_ident(size);
                        self.visit_ident(init);
                    }
                    ExprKind::Get(array, index) => {
                        self.visit_ident(array);
                        self.visit_ident(index);
                    }
                    ExprKind::Set(array, index, value) => {
                        self.visit_ident(array);
                        self.visit_ident(index);
                        self.visit_ident(value);
                    }
                }
            }

            fn visit_ident(&mut self, ident: & $($mutability)? Ident<'ctx>) {
                self.super_ident(ident);
            }

            fn super_ident(&mut self, _ident: & $($mutability)? Ident<'ctx>) {}

            fn visit_function_instance(&mut self, instance: & $($mutability)? FunctionInstance<'ctx>) {
                self.super_function_instance(instance);
            }

            fn super_function_instance(&mut self, instance: & $($mutability)? FunctionInstance<'ctx>) {
                match instance {
                    FunctionInstance::Defined(_) => (),
                    FunctionInstance::Imported(name) => {
                        self.visit_imported_function(name);
                    }
                }
            }

            fn visit_imported_function(&mut self, name: & $($mutability)? Interned<'ctx, str>) {
                self.super_imported_function(name);
            }

            fn super_imported_function(&mut self, _name: & $($mutability)? Interned<'ctx, str>) {}
        }
    };
}

declare_visitor!(Visitor,);
declare_visitor!(MutVisitor, mut);
