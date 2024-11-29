use data_structure::index::vec::IndexVec;
use ir_asm_virtual::{Local, StmtKind, TerminatorKind, TupleIndex};

use crate::VirtAsmPass;

/// Removes all unit typed values from the program.
///
/// This may invalidate:
/// - [`ir_asm_virtual::ArgIndex`]
/// - [`ir_asm_virtual::TupleIndex`]
///
/// but keeps:
/// - [`ir_asm_virtual::Local`]
/// - [`ir_asm_virtual::StmtIndex`]
pub struct RemoveUnit;

impl<'ctx> VirtAsmPass<'ctx> for RemoveUnit {
    fn run_pass(
        &mut self,
        _ctx: &'ctx middleware::GlobalContext<'ctx>,
        program: &mut ir_asm_virtual::Program<'ctx>,
    ) {
        use ir_asm_virtual::MutVisitor;
        struct RemoveUnitVisitor<'ctx, 'a> {
            non_unit_locals: &'a IndexVec<Local, bool>,
            _phantom: std::marker::PhantomData<&'ctx ()>,
        }

        impl<'ctx, 'a> RemoveUnitVisitor<'ctx, 'a> {
            fn new(non_unit_locals: &'a IndexVec<Local, bool>) -> Self {
                Self {
                    non_unit_locals,
                    _phantom: std::marker::PhantomData,
                }
            }
        }

        impl<'ctx> MutVisitor<'ctx> for RemoveUnitVisitor<'ctx, '_> {
            fn visit_stmt(&mut self, stmt: &mut StmtKind<'ctx>) {
                match stmt {
                    StmtKind::Nop => (),
                    StmtKind::Assign { place, value } => {
                        self.visit_expr(value);
                        if value.is_unit() {
                            stmt.make_nop();
                        } else if value.ty.is_unit() {
                            *place = None;
                        }
                    }
                };
            }

            fn visit_tuple(&mut self, elems: &mut IndexVec<TupleIndex, Local>) {
                elems.retain(|local| self.non_unit_locals.contains(*local));
            }

            fn visit_block_arg(&mut self, args: &mut IndexVec<ir_asm_virtual::ArgIndex, Local>) {
                args.retain(|local| self.non_unit_locals.contains(*local));
            }

            fn visit_terminator(&mut self, terminator: &mut TerminatorKind<'ctx>) {
                match terminator {
                    TerminatorKind::Return(index_vec) => {
                        index_vec.retain(|local| self.non_unit_locals.contains(*local));
                    }
                    TerminatorKind::Branch(branch) => {
                        branch
                            .args
                            .retain(|local| self.non_unit_locals.contains(*local));
                    }
                    TerminatorKind::ConditionalBranch {
                        targets,
                        condition: _,
                    } => {
                        targets.iter_mut().for_each(|target| {
                            target
                                .args
                                .retain(|local| self.non_unit_locals.contains(*local))
                        });
                    }
                    TerminatorKind::Call {
                        calling_conv: _,
                        args,
                        branch,
                    } => {
                        args.retain(|local| self.non_unit_locals.contains(*local));
                        branch
                            .args
                            .retain(|local| self.non_unit_locals.contains(*local));
                    }
                }
            }
        }

        for function in &mut program.functions {
            let non_unit_locals = function
                .local_decls
                .iter()
                .map(|decl| !decl.ty().is_unit())
                .collect();
            RemoveUnitVisitor::new(&non_unit_locals).visit_function_def(function);
        }
    }
}
