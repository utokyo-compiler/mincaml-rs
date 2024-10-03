use anyhow::Result;
use wasm_encoder::{Instruction, MemArg};

use crate::{
    function,
    index::LocalIdx,
    program::{self, FnTypeSignature},
    ty::{WasmPrimitiveTy, WasmTy},
};

/// The global index of the heap pointer.
const HEAP_PTR: u32 = 0;

pub fn codegen<'ctx>(
    program_state: &mut program::State<'_, 'ctx>,
    function_state: &mut function::State<'ctx>,
    expr: &ir_closure::Expr<'ctx>,
) -> Result<()> {
    const MEM_ARG: MemArg = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };

    match expr.kind() {
        ir_closure::ExprKind::Const(lit_kind) => {
            let instr = match lit_kind {
                ir_closure::LitKind::Unit => Instruction::I32Const(0),
                ir_closure::LitKind::Bool(b) => Instruction::I32Const(if *b { 1 } else { 0 }),
                ir_closure::LitKind::Int(i) => Instruction::I32Const(*i),
                ir_closure::LitKind::Float(f_bits) => {
                    Instruction::F32Const(f32::from_bits(*f_bits))
                }
            };
            function_state.push_raw(instr);
        }
        ir_closure::ExprKind::Unary(un_op, e1) => {
            let local = function_state.get_local(*e1).expect_single()?;

            match un_op {
                ir_closure::UnOp::Neg => {
                    function_state.push_raw(Instruction::I32Const(0));
                    function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
                    function_state.push_raw(Instruction::I32Sub);
                }
                ir_closure::UnOp::Not => {
                    function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
                    function_state.push_raw(Instruction::I32Eqz);
                }
                ir_closure::UnOp::FNeg => {
                    function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
                    function_state.push_raw(Instruction::F32Neg);
                }
            };
        }
        ir_closure::ExprKind::Binary(bin_op, e1, e2) => {
            let local1 = function_state.get_local(*e1).expect_single()?;
            let local2 = function_state.get_local(*e2).expect_single()?;

            match bin_op {
                ir_closure::BinOp::Relation(bbin_op_kind) => {
                    function_state.push_raw(Instruction::LocalGet(local1.unwrap_idx()));
                    function_state.push_raw(Instruction::LocalGet(local2.unwrap_idx()));

                    let instr = match bbin_op_kind {
                        ir_closure::RelationBinOpKind::Eq => Instruction::I32Eq,
                        ir_closure::RelationBinOpKind::Ne => Instruction::I32Ne,
                        ir_closure::RelationBinOpKind::Lt => Instruction::I32LtS,
                        ir_closure::RelationBinOpKind::Le => Instruction::I32LeS,
                        ir_closure::RelationBinOpKind::Gt => Instruction::I32GtS,
                        ir_closure::RelationBinOpKind::Ge => Instruction::I32GeS,
                    };
                    function_state.push_raw(instr);
                }
                ir_closure::BinOp::Int(ibin_op_kind) => {
                    function_state.push_raw(Instruction::LocalGet(local1.unwrap_idx()));
                    function_state.push_raw(Instruction::LocalGet(local2.unwrap_idx()));

                    let instr = match ibin_op_kind {
                        ir_closure::IntBinOpKind::Add => Instruction::I32Add,
                        ir_closure::IntBinOpKind::Sub => Instruction::I32Sub,
                        ir_closure::IntBinOpKind::Mul => Instruction::I32Mul,
                        ir_closure::IntBinOpKind::Div => Instruction::I32DivS,
                    };
                    function_state.push_raw(instr);
                }
                ir_closure::BinOp::Float(fbin_op_kind) => {
                    function_state.push_raw(Instruction::LocalGet(local1.unwrap_idx()));
                    function_state.push_raw(Instruction::LocalGet(local2.unwrap_idx()));

                    let instr = match fbin_op_kind {
                        ir_closure::FloatBinOpKind::FAdd => Instruction::F32Add,
                        ir_closure::FloatBinOpKind::FSub => Instruction::F32Sub,
                        ir_closure::FloatBinOpKind::FMul => Instruction::F32Mul,
                        ir_closure::FloatBinOpKind::FDiv => Instruction::F32Div,
                    };
                    function_state.push_raw(instr);
                }
            }
        }
        ir_closure::ExprKind::If(cond, then_expr, else_expr) => {
            let local = function_state.get_local(*cond);
            function_state.push_raw(Instruction::LocalGet(local.expect_single()?.unwrap_idx()));
            let typeidx = program_state
                .signature_interner
                .intern(FnTypeSignature::from_results(
                    WasmTy::from_ty(expr.ty).into_iter_primitives().collect(),
                ));
            function_state.push_raw(Instruction::If(wasm_encoder::BlockType::FunctionType(
                typeidx.unwrap_idx(),
            )));
            codegen(program_state, function_state, then_expr)?;
            function_state.push_raw(Instruction::Else);
            codegen(program_state, function_state, else_expr)?;
            function_state.push_raw(Instruction::End);
        }
        ir_closure::ExprKind::Let(ir_closure::LetBinding { pattern, value }, follows) => {
            // Do `codegen` on `value` first to ensure that the value is on the stack before we bind it.
            codegen(program_state, function_state, value)?;

            match pattern {
                ir_closure::Pattern::Unit => {
                    function_state.push_raw(Instruction::Drop);
                }
                ir_closure::Pattern::Var(var) => {
                    let local = function_state.get_local(*var);
                    for local in local.iter() {
                        function_state.push_raw(Instruction::LocalSet(local.unwrap_idx()));
                    }
                }
                ir_closure::Pattern::Tuple(vars) => {
                    let local = function_state.new_local(WasmPrimitiveTy::I32);
                    function_state.push_raw(Instruction::LocalSet(local.unwrap_idx()));

                    let mut mem_arg = MEM_ARG;
                    for var in vars {
                        let local = function_state.get_local(*var);
                        function_state
                            .push_raw(Instruction::LocalSet(local.expect_single()?.unwrap_idx()));
                        let wasm_ty = WasmTy::from_ty(var.ty);
                        let prim_ty = wasm_ty.as_primitive().ok_or_else(|| {
                            anyhow::anyhow!("closure cannot be stored in a tuple")
                        })?;
                        match prim_ty {
                            WasmPrimitiveTy::I32 => {
                                function_state.push_raw(Instruction::I32Load(mem_arg))
                            }
                            WasmPrimitiveTy::F32 => {
                                function_state.push_raw(Instruction::F32Load(mem_arg))
                            }
                            WasmPrimitiveTy::RefFn => {
                                return Err(anyhow::anyhow!("closure cannot be stored in a tuple"));
                            }
                        };
                        mem_arg.offset += prim_ty.size_of() as u64;
                    }
                }
            };
            codegen(program_state, function_state, follows)?;
        }
        ir_closure::ExprKind::Var(ident) => {
            let local = function_state.get_local(*ident);
            for local in local.iter() {
                function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
            }
        }
        ir_closure::ExprKind::ClosureMake(closure) => {
            function_state.push_raw(Instruction::RefFunc(
                program_state.get_func_idx(closure.function).unwrap_idx(),
            ));
        }
        ir_closure::ExprKind::App(ir_closure::ApplyKind::Closure { ident }, args) => {
            let local = function_state.get_local(*ident);
            let local_group = local.as_many().unwrap();
            let function = &local_group[0];
            let closure_args = &local_group[1..];
            // closure calling convention
            const TABLE_IDX: u32 = 0;
            function_state.push_raw(Instruction::LocalGet(function.unwrap_idx()));
            function_state.push_raw(Instruction::TableSet(TABLE_IDX));
            for closure_arg in closure_args {
                function_state.push_raw(Instruction::LocalGet(closure_arg.unwrap_idx()));
            }
            for arg in args {
                let local = function_state.get_local(*arg);
                for local in local.iter() {
                    function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
                }
            }
            let type_idx = program_state
                .signature_interner
                .intern(FnTypeSignature::from_results(
                    WasmTy::from_ty(ident.ty).into_iter_primitives().collect(),
                ));
            function_state.push_raw(Instruction::CallIndirect {
                type_index: type_idx.unwrap_idx(),
                table_index: TABLE_IDX,
            });
        }
        ir_closure::ExprKind::App(ir_closure::ApplyKind::Direct { function }, args) => {
            for arg in args {
                let local = function_state.get_local(*arg);
                for local in local.iter() {
                    function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
                }
            }
            function_state.push_raw(Instruction::Call(
                program_state.get_func_idx(*function).unwrap_idx(),
            ));
        }
        ir_closure::ExprKind::Tuple(vars) => {
            let mut mem_arg = MEM_ARG;
            for var in vars {
                function_state.push_raw(Instruction::GlobalGet(HEAP_PTR));
                let local = function_state.get_local(*var).expect_single()?;
                function_state.push_raw(Instruction::LocalGet(local.unwrap_idx()));
                match WasmTy::from_ty(var.ty)
                    .as_primitive()
                    .ok_or_else(|| anyhow::anyhow!("closure cannot be stored in a tuple"))?
                {
                    WasmPrimitiveTy::I32 => function_state.push_raw(Instruction::I32Store(mem_arg)),
                    WasmPrimitiveTy::F32 => function_state.push_raw(Instruction::F32Store(mem_arg)),
                    WasmPrimitiveTy::RefFn => {
                        return Err(anyhow::anyhow!("closure cannot be stored in a tuple"));
                    }
                };
                mem_arg.offset += WasmTy::from_ty(var.ty)
                    .as_primitive()
                    .ok_or_else(|| anyhow::anyhow!("closure cannot be stored in a tuple"))?
                    .size_of() as u64;
            }
            grow_heap(function_state, mem_arg.offset as i32);
        }
        ir_closure::ExprKind::ArrayMake(len, init) => {
            let local_len = function_state.get_local(*len).expect_single()?;
            let local_init = function_state.get_local(*init).expect_single()?;
            let wasm_ty = function_state.get_local_decl(local_init).wasm_ty;

            // return the address of the array
            function_state.push_raw(Instruction::GlobalGet(HEAP_PTR));

            function_state.push_raw(Instruction::Block(wasm_encoder::BlockType::Empty));
            {
                function_state.push_raw(Instruction::LocalGet(local_len.unwrap_idx()));
                function_state.push_raw(Instruction::I32Eqz);
                function_state.push_raw(Instruction::BrIf(0));

                // initial value is always zero
                let loop_counter = function_state.new_local(WasmPrimitiveTy::I32);

                function_state.push_raw(Instruction::Loop(wasm_encoder::BlockType::Empty));
                {
                    function_state.push_raw(Instruction::LocalGet(loop_counter.unwrap_idx()));
                    function_state.push_raw(Instruction::I32Const(1));
                    function_state.push_raw(Instruction::I32Add);
                    function_state.push_raw(Instruction::LocalSet(loop_counter.unwrap_idx()));

                    function_state.push_raw(Instruction::GlobalGet(HEAP_PTR));
                    function_state.push_raw(Instruction::LocalGet(local_init.unwrap_idx()));
                    match wasm_ty {
                        WasmPrimitiveTy::I32 => {
                            function_state.push_raw(Instruction::I32Store(MEM_ARG));
                        }
                        WasmPrimitiveTy::F32 => {
                            function_state.push_raw(Instruction::F32Store(MEM_ARG));
                        }
                        WasmPrimitiveTy::RefFn => unreachable!(),
                    }
                    grow_heap(function_state, wasm_ty.size_of() as i32);

                    // if `loop_counter` <= `len`, continue
                    function_state.push_raw(Instruction::LocalGet(loop_counter.unwrap_idx()));
                    function_state.push_raw(Instruction::LocalGet(local_len.unwrap_idx()));
                    function_state.push_raw(Instruction::I32GeS);
                    function_state.push_raw(Instruction::BrIf(0));

                    function_state.push_raw(Instruction::End);
                }

                function_state.push_raw(Instruction::End);
            }
        }
        ir_closure::ExprKind::Get(base, index) => {
            let local_base = function_state.get_local(*base).expect_single()?;
            let base_ty = function_state.get_local_decl(local_base).wasm_ty;
            let local_index = function_state.get_local(*index).expect_single()?;

            calc_addr(function_state, local_base, local_index, base_ty);

            function_state.push_raw(Instruction::I32Load(MEM_ARG));
        }
        ir_closure::ExprKind::Set(base, index, value) => {
            let local_base = function_state.get_local(*base).expect_single()?;
            let base_ty = function_state.get_local_decl(local_base).wasm_ty;
            let local_index = function_state.get_local(*index).expect_single()?;
            let local_value = function_state.get_local(*value).expect_single()?;

            calc_addr(function_state, local_base, local_index, base_ty);

            function_state.push_raw(Instruction::LocalGet(local_value.unwrap_idx()));
            function_state.push_raw(Instruction::I32Store(MEM_ARG));
        }
    };
    Ok(())
}

/// Grow the heap pointer by the given size.
fn grow_heap(function_state: &mut function::State<'_>, size: i32) {
    function_state.push_raw(Instruction::GlobalGet(HEAP_PTR));
    function_state.push_raw(Instruction::I32Const(size));
    function_state.push_raw(Instruction::I32Add);
    function_state.push_raw(Instruction::GlobalSet(HEAP_PTR));
}

/// Compute the address of the element.
fn calc_addr(
    function_state: &mut function::State<'_>,
    local_base: LocalIdx,
    local_index: LocalIdx,
    base_ty: WasmPrimitiveTy,
) {
    function_state.push_raw(Instruction::LocalGet(local_base.unwrap_idx()));
    function_state.push_raw(Instruction::LocalGet(local_index.unwrap_idx()));
    function_state.push_raw(Instruction::I32Const(base_ty.size_of() as i32));
    function_state.push_raw(Instruction::I32Mul);
    function_state.push_raw(Instruction::I32Add);
}
