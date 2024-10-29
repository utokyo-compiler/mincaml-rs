use anyhow::Result;
use wasm_encoder::Instruction;

use crate::{
    constant::{HEAP_PTR, MEM_ARG, TABLE_IDX},
    function,
    program::{self, FnTypeSignature},
    ty::WasmTy,
};

pub fn codegen<'ctx>(
    program_state: &mut program::State<'_, 'ctx>,
    function_state: &mut function::State<'ctx>,
    expr: &ir_closure::Expr<'ctx>,
) -> Result<()> {
    match expr.kind() {
        ir_closure::ExprKind::Const(lit_kind) => {
            match lit_kind {
                ir_closure::LitKind::Unit => {
                    // do nothing
                }
                ir_closure::LitKind::Bool(b) => {
                    function_state
                        .instrs
                        .push(Instruction::I32Const(if *b { 1 } else { 0 }));
                }
                ir_closure::LitKind::Int(i) => {
                    function_state.instrs.push(Instruction::I32Const(*i));
                }
                ir_closure::LitKind::Float(f_bits) => {
                    function_state
                        .instrs
                        .push(Instruction::F32Const(f32::from_bits(*f_bits)));
                }
            };
        }
        ir_closure::ExprKind::Unary(un_op, e1) => {
            let local = function_state.local_def.get(*e1).unwrap();

            match un_op {
                ir_closure::UnOp::Ineg => {
                    function_state.instrs.push(Instruction::I32Const(0));
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local.unwrap_idx()));
                    function_state.instrs.push(Instruction::I32Sub);
                }
                ir_closure::UnOp::Not => {
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local.unwrap_idx()));
                    function_state.instrs.push(Instruction::I32Eqz);
                }
                ir_closure::UnOp::Fneg => {
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local.unwrap_idx()));
                    function_state.instrs.push(Instruction::F32Neg);
                }
            };
        }
        ir_closure::ExprKind::Binary(bin_op, e1, e2) => {
            let local1 = function_state.local_def.get(*e1).unwrap();
            let local2 = function_state.local_def.get(*e2).unwrap();

            match bin_op {
                ir_closure::BinOp::Relation(bbin_op_kind) => {
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local1.unwrap_idx()));
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local2.unwrap_idx()));

                    let instr = match bbin_op_kind {
                        ir_closure::RelationBinOpKind::Eq => Instruction::I32Eq,
                        ir_closure::RelationBinOpKind::Ne => Instruction::I32Ne,
                        ir_closure::RelationBinOpKind::Lt => Instruction::I32LtS,
                        ir_closure::RelationBinOpKind::Le => Instruction::I32LeS,
                        ir_closure::RelationBinOpKind::Gt => Instruction::I32GtS,
                        ir_closure::RelationBinOpKind::Ge => Instruction::I32GeS,
                    };
                    function_state.instrs.push(instr);
                }
                ir_closure::BinOp::Int(ibin_op_kind) => {
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local1.unwrap_idx()));
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local2.unwrap_idx()));

                    let instr = match ibin_op_kind {
                        ir_closure::IntBinOpKind::Add => Instruction::I32Add,
                        ir_closure::IntBinOpKind::Sub => Instruction::I32Sub,
                        ir_closure::IntBinOpKind::Mul => Instruction::I32Mul,
                        ir_closure::IntBinOpKind::Div => Instruction::I32DivS,
                    };
                    function_state.instrs.push(instr);
                }
                ir_closure::BinOp::Float(fbin_op_kind) => {
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local1.unwrap_idx()));
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local2.unwrap_idx()));

                    let instr = match fbin_op_kind {
                        ir_closure::FloatBinOpKind::FAdd => Instruction::F32Add,
                        ir_closure::FloatBinOpKind::FSub => Instruction::F32Sub,
                        ir_closure::FloatBinOpKind::FMul => Instruction::F32Mul,
                        ir_closure::FloatBinOpKind::FDiv => Instruction::F32Div,
                    };
                    function_state.instrs.push(instr);
                }
            }
        }
        ir_closure::ExprKind::If(cond, then_expr, else_expr) => {
            let local = function_state.local_def.get(*cond).unwrap();
            function_state
                .instrs
                .push(Instruction::LocalGet(local.unwrap_idx()));
            let type_index = program_state
                .signature_interner
                .intern(FnTypeSignature::from_results(WasmTy::from_ty(expr.ty)));
            function_state
                .instrs
                .push(Instruction::If(wasm_encoder::BlockType::FunctionType(
                    type_index.unwrap_idx(),
                )));
            codegen(program_state, function_state, then_expr)?;
            function_state.instrs.push(Instruction::Else);
            codegen(program_state, function_state, else_expr)?;
            function_state.instrs.push(Instruction::End);
        }
        ir_closure::ExprKind::Let(ir_closure::LetBinding { pattern, value }, follows) => {
            // Do `codegen` on `value` first to ensure that the value is on the stack before we bind it.
            codegen(program_state, function_state, value)?;

            match pattern {
                ir_closure::Pattern::Unit => {
                    // do nothing
                }
                ir_closure::Pattern::Var(var) => {
                    let local = function_state.local_def.get(*var);
                    if let Some(local) = local {
                        function_state
                            .instrs
                            .push(Instruction::LocalSet(local.unwrap_idx()));
                    }
                }
                ir_closure::Pattern::Tuple(vars) => {
                    let local = function_state.local_def.new_local(WasmTy::I32);
                    function_state
                        .instrs
                        .push(Instruction::LocalSet(local.unwrap_idx()));

                    function_state.instrs_load_from_tuple(vars.iter().copied(), local, MEM_ARG);
                }
            };

            // Continue with the rest of the code.
            codegen(program_state, function_state, follows)?;
        }
        ir_closure::ExprKind::Var(ident) => {
            let local = function_state.local_def.get(*ident);
            if let Some(local) = local {
                function_state
                    .instrs
                    .push(Instruction::LocalGet(local.unwrap_idx()));
            }
        }
        ir_closure::ExprKind::ClosureMake(closure) => {
            let local = function_state.local_def.new_local(WasmTy::I32);

            // Here, we store the function index of the closure in
            // the local variable **as a table index**.

            // This transmutation should be synchronized with
            // the table initialization in the element section.
            function_state.instrs.push(Instruction::I32Const(
                program_state.get_func_idx(closure.function).unwrap_idx() as i32,
            ));
            function_state
                .instrs
                .push(Instruction::LocalSet(local.unwrap_idx()));

            // Store the captured arguments in the tuple.
            let vars: Vec<_> = closure
                .captured_args
                .iter()
                .filter_map(|var| function_state.local_def.get_typed(*var))
                .collect();
            function_state.instrs_allocate_tuple(std::iter::once((local, WasmTy::I32)).chain(vars));
        }
        ir_closure::ExprKind::App(ir_closure::ApplyKind::Closure { ident }, args) => {
            // closure calling convention

            // Load the arguments.
            for local in args
                .iter()
                .filter_map(|arg| function_state.local_def.get(*arg))
            {
                function_state
                    .instrs
                    .push(Instruction::LocalGet(local.unwrap_idx()));
            }

            // Load the closure thunk pointer.
            let local = function_state.local_def.get(*ident).unwrap();
            function_state
                .instrs
                .push(Instruction::LocalGet(local.unwrap_idx()));

            // Load the function pointer.
            function_state
                .instrs
                .push(Instruction::LocalGet(local.unwrap_idx()));
            function_state.instrs.push(Instruction::I32Load(MEM_ARG));

            // Call the function via the table.
            function_state.instrs.push(Instruction::CallIndirect {
                type_index: program_state
                    .signature_interner
                    .intern({
                        // The signature of the function is not the same to the
                        // type of the closure because the last argument is the
                        // thunk pointer.
                        let mut signature = FnTypeSignature::from_fun_ty(ident.ty);
                        signature.params.push(WasmTy::I32);
                        signature
                    })
                    .unwrap_idx(),
                table_index: TABLE_IDX,
            });
        }
        ir_closure::ExprKind::App(ir_closure::ApplyKind::Direct { function }, args) => {
            // Load the arguments.
            for local in args
                .iter()
                .filter_map(|arg| function_state.local_def.get(*arg))
            {
                function_state
                    .instrs
                    .push(Instruction::LocalGet(local.unwrap_idx()));
            }

            // Call the function directly.
            function_state.instrs.push(Instruction::Call(
                program_state.get_func_idx(*function).unwrap_idx(),
            ));
        }
        ir_closure::ExprKind::Tuple(vars) => {
            let vars: Vec<_> = vars
                .iter()
                .filter_map(|var| function_state.local_def.get_typed(*var))
                .collect();
            function_state.instrs_allocate_tuple(vars.into_iter());
        }
        ir_closure::ExprKind::ArrayMake(len, init) => {
            // return the address of the array
            function_state.instrs.push(Instruction::GlobalGet(HEAP_PTR));

            if init.ty.is_unit() {
                return Ok(());
            }

            let local_len = function_state.local_def.get(*len).unwrap();
            let (local_init, wasm_ty) = function_state.local_def.get_typed(*init).unwrap();

            function_state
                .instrs
                .push(Instruction::Block(wasm_encoder::BlockType::Empty));
            {
                function_state
                    .instrs
                    .push(Instruction::LocalGet(local_len.unwrap_idx()));
                function_state.instrs.push(Instruction::I32Eqz);
                function_state.instrs.push(Instruction::BrIf(0));

                // initial value is always zero
                let loop_counter = function_state.local_def.new_local(WasmTy::I32);

                function_state
                    .instrs
                    .push(Instruction::Loop(wasm_encoder::BlockType::Empty));
                {
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(loop_counter.unwrap_idx()));
                    function_state.instrs.push(Instruction::I32Const(1));
                    function_state.instrs.push(Instruction::I32Add);
                    function_state
                        .instrs
                        .push(Instruction::LocalSet(loop_counter.unwrap_idx()));

                    function_state.instrs.push(Instruction::GlobalGet(HEAP_PTR));
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local_init.unwrap_idx()));
                    match wasm_ty {
                        WasmTy::I32 => {
                            function_state.instrs.push(Instruction::I32Store(MEM_ARG));
                        }
                        WasmTy::F32 => {
                            function_state.instrs.push(Instruction::F32Store(MEM_ARG));
                        }
                    }
                    function_state.instrs_grow_heap(wasm_ty.size_of() as i32);

                    // if `loop_counter` <= `len`, continue
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(loop_counter.unwrap_idx()));
                    function_state
                        .instrs
                        .push(Instruction::LocalGet(local_len.unwrap_idx()));
                    function_state.instrs.push(Instruction::I32GeS);
                    function_state.instrs.push(Instruction::BrIf(0));

                    function_state.instrs.push(Instruction::End);
                }

                function_state.instrs.push(Instruction::End);
            }
        }
        ir_closure::ExprKind::Get(base, index) => {
            let Some(inner) = base.ty.as_array() else {
                return Err(anyhow::anyhow!("expected an array"));
            };
            if inner.is_unit() {
                // do nothing
                return Ok(());
            }
            let (local_base, base_ty) = function_state.local_def.get_typed(*base).unwrap();
            let local_index = function_state.local_def.get(*index).unwrap();

            function_state.instrs_calc_addr(local_base, local_index, base_ty);

            function_state.instrs.push(Instruction::I32Load(MEM_ARG));
        }
        ir_closure::ExprKind::Set(base, index, value) => {
            let (local_base, base_ty) = function_state.local_def.get_typed(*base).unwrap();
            let local_index = function_state.local_def.get(*index).unwrap();
            let local_value = function_state.local_def.get(*value).unwrap();

            function_state.instrs_calc_addr(local_base, local_index, base_ty);

            function_state
                .instrs
                .push(Instruction::LocalGet(local_value.unwrap_idx()));
            function_state.instrs.push(Instruction::I32Store(MEM_ARG));
        }
    };
    // do not add code here, because the code above may early return
    Ok(())
}
