use crate::*;
use core::range::Range;
use data_structure::index::vec::IndexVec;

macro_rules! overload_mut {
    ($receiver:ident, [terminator],) => {
        $receiver.terminator()
    };
    ($receiver:ident, [terminator], mut) => {
        $receiver.terminator_mut()
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
                for block in & $($mutability)? function.basic_blocks {
                    self.visit_block_data(block);
                }
            }

            fn visit_function(&mut self, function: & $($mutability)? FunctionInstance<'ctx>) {
                self.super_function(function);
            }

            fn super_function(&mut self, _function: & $($mutability)? FunctionInstance<'ctx>) {}

            fn visit_block_data(&mut self, block: & $($mutability)? BasicBlockData<'ctx>) {
                self.super_block_data(block);
            }

            /// This method is provided one and not intended to be overridden.
            fn super_ranged_block_data(&mut self, basic_blocks: & $($mutability)? IndexVec<BasicBlock, BasicBlockData<'ctx>>, range: Range<BasicBlock>) {
                for block in & $($mutability)? basic_blocks[range] {
                    self.visit_block_data(block);
                }
            }

            fn super_block_data(&mut self, block: & $($mutability)? BasicBlockData<'ctx>) {
                for arg in & $($mutability)? block.args {
                    self.visit_local(arg, LocalVisitContext::AssignLhs);
                }
                for stmt in & $($mutability)? block.stmts {
                    self.visit_stmt(stmt);
                }
                self.visit_terminator(overload_mut!(block, [terminator], $($mutability)?));
            }

            fn visit_stmt(&mut self, stmt: & $($mutability)? StmtKind<'ctx>) {
                self.super_stmt(stmt);
            }

            fn super_stmt(&mut self, stmt: & $($mutability)? StmtKind<'ctx>) {
                match stmt {
                    StmtKind::Nop => (),
                    StmtKind::Assign {
                        place,
                        value,
                    } => {
                        self.visit_place(place, LocalVisitContext::AssignLhs);
                        self.visit_expr(value);
                    }
                }
            }

            fn visit_place(&mut self, place: & $($mutability)? Place, context: LocalVisitContext) {
                self.super_place(place, context);
            }

            fn super_place(&mut self, place: & $($mutability)? Place, context: LocalVisitContext) {
                match place {
                    Place::Discard => (),
                    Place::Local(local) => self.visit_local(local, context),
                    Place::Projection { base, projection_kind } => {
                        self.visit_projection(base, projection_kind, context);
                    }
                }
            }

            fn visit_local(&mut self, local: & $($mutability)? Local, context: LocalVisitContext) {
                self.super_local(local, context);
            }

            fn super_local(&mut self, _local: & $($mutability)? Local, _context: LocalVisitContext) {}

            fn visit_projection(&mut self, base: & $($mutability)? Local, projection_kind: & $($mutability)? ProjectionKind, context: LocalVisitContext) {
                self.super_projection(base, projection_kind, context);
            }

            fn super_projection(&mut self, base: & $($mutability)? Local, _projection_kind: & $($mutability)? ProjectionKind, context: LocalVisitContext) {
                self.visit_local(base, context);
            }

            fn visit_expr(&mut self, expr: & $($mutability)? ExprKind<'ctx>) {
                self.super_expr(expr);
            }

            fn super_expr(&mut self, expr: & $($mutability)? ExprKind<'ctx>) {
                match expr {
                    ExprKind::Const(_) => (),
                    ExprKind::Unary(_, e) => self.visit_local(e, LocalVisitContext::Use),
                    ExprKind::Binary(_, e1, e2) => {
                        self.visit_local(e1, LocalVisitContext::Use);
                        self.visit_local(e2, LocalVisitContext::Use);
                    }
                    ExprKind::ClosureMake(closure) => {
                        self.visit_closure_make(closure);
                    }
                    ExprKind::Tuple(es) => {
                        for e in es {
                            self.visit_local(e, LocalVisitContext::Use);
                        }
                    }
                    ExprKind::ArrayMake(e1, e2) => {
                        self.visit_local(e1, LocalVisitContext::Use);
                        self.visit_local(e2, LocalVisitContext::Use);
                    }
                    ExprKind::Read(place) => {
                        self.visit_place(place, LocalVisitContext::Use);
                    }
                }
            }

            fn visit_closure_make(&mut self, closure: & $($mutability)? Closure<'ctx>) {
                self.super_closure_make(closure);
            }

            fn super_closure_make(&mut self, closure: & $($mutability)? Closure<'ctx>) {
                self.visit_function(& $($mutability)? closure.function);
                for captured_arg in & $($mutability)? closure.captured_args {
                    self.visit_local(captured_arg, LocalVisitContext::Use);
                }
            }

            fn visit_terminator(&mut self, terminator: & $($mutability)? TerminatorKind<'ctx>) {
                self.super_terminator(terminator);
            }

            fn super_terminator(&mut self, terminator: & $($mutability)? TerminatorKind<'ctx>) {
                match terminator {
                    TerminatorKind::Return(args) => {
                        for arg in args {
                            self.visit_local(arg, LocalVisitContext::Use);
                        }
                    },
                    TerminatorKind::Branch(branch) => {
                        self.visit_branch(branch);
                    }
                    TerminatorKind::ConditionalBranch { condition, targets } => {
                        self.visit_local(condition, LocalVisitContext::Use);
                        for target in targets {
                            self.visit_basic_block(target);
                        }
                    }
                    TerminatorKind::Call {
                        calling_conv,
                        args,
                        branch,
                    } => {
                        self.visit_calling_conv(calling_conv);
                        for arg in args {
                            self.visit_local(arg, LocalVisitContext::Use);
                        }
                        self.visit_branch(branch);
                    }
                }
            }

            fn visit_branch(&mut self, branch: & $($mutability)? Branch) {
                self.super_branch(branch);
            }

            fn super_branch(&mut self, branch: & $($mutability)? Branch) {
                self.visit_basic_block(& $($mutability)? branch.target);
                for arg in & $($mutability)? branch.args {
                    self.visit_local(arg, LocalVisitContext::Use);
                }
            }

            fn visit_basic_block(&mut self, basic_block: & $($mutability)? BasicBlock) {
                self.super_basic_block(basic_block);
            }

            fn super_basic_block(&mut self, _basic_block: & $($mutability)? BasicBlock) {}

            fn visit_calling_conv(&mut self, calling_conv: & $($mutability)? AbsCallingConv) {
                self.super_calling_conv(calling_conv);
            }

            fn super_calling_conv(&mut self, _calling_conv: & $($mutability)? AbsCallingConv) {}
        }
    };
}

declare_visitor!(Visitor,);
declare_visitor!(MutVisitor, mut);

pub enum LocalVisitContext {
    AssignLhs,
    Use,
}
