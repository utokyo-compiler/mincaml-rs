use data_structure::index::vec::IndexVec;
use ir_asm_virtual::Local;

use crate::VirtAsmPass;

/// Remove no-op statements and unused definitions.
///
/// This may invalidate:
/// - [`ir_asm_virtual::StmtIndex`]
///
/// but keeps:
/// - [`ir_asm_virtual::Local`]
/// - [`ir_asm_virtual::ArgIndex`]
/// - [`ir_asm_virtual::TupleIndex`]
pub struct RemoveUnused;

impl<'ctx> VirtAsmPass<'ctx> for RemoveUnused {
    fn run_pass(
        &mut self,
        _ctx: &'ctx middleware::GlobalContext<'ctx>,
        program: &mut ir_asm_virtual::Program<'ctx>,
    ) {
        use ir_asm_virtual::MutVisitor;
        use ir_asm_virtual::Visitor;
        struct MarkVisitor<'ctx, 'a> {
            used_locals: &'a mut IndexVec<Local, bool>,
            _phantom: std::marker::PhantomData<&'ctx ()>,
        }

        impl<'ctx, 'a> MarkVisitor<'ctx, 'a> {
            fn new(used_locals: &'a mut IndexVec<Local, bool>) -> Self {
                Self {
                    used_locals,
                    _phantom: std::marker::PhantomData,
                }
            }
        }

        impl<'ctx> Visitor<'ctx> for MarkVisitor<'ctx, '_> {
            fn visit_local(&mut self, local: &Local, context: ir_asm_virtual::LocalVisitContext) {
                if let ir_asm_virtual::LocalVisitContext::Use = context {
                    self.used_locals[*local] = true;
                }
            }
        }

        struct SweepVisitor<'ctx, 'a> {
            used_locals: &'a IndexVec<Local, bool>,
            _phantom: std::marker::PhantomData<&'ctx ()>,
        }

        impl<'ctx, 'a> SweepVisitor<'ctx, 'a> {
            fn new(used_locals: &'a IndexVec<Local, bool>) -> Self {
                Self {
                    used_locals,
                    _phantom: std::marker::PhantomData,
                }
            }
        }

        impl<'ctx> MutVisitor<'ctx> for SweepVisitor<'ctx, '_> {
            fn visit_block_arg(&mut self, args: &mut IndexVec<ir_asm_virtual::ArgIndex, Local>) {
                args.retain(|local| self.used_locals[*local]);
            }

            fn visit_stmts(
                &mut self,
                stmts: &mut IndexVec<ir_asm_virtual::StmtIndex, ir_asm_virtual::StmtKind<'ctx>>,
            ) {
                stmts.retain(|stmt| {
                    stmt.has_effect()
                        || stmt
                            .local()
                            .map(|local| self.used_locals.contains(local))
                            .unwrap_or_default()
                })
            }
        }

        for function in &mut program.functions {
            let mut used_locals = function.local_decls.iter().map(|_| false).collect();
            MarkVisitor::new(&mut used_locals).visit_function_def(function);
            SweepVisitor::new(&used_locals).visit_function_def(function);
        }
    }
}
