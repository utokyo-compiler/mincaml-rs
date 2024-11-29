use std::fmt::Display;

use crate::{
    BinOp, ExprKind, FloatBinOpKind, IntBinOpKind, LetBinder, LitKind, Pattern, RelationBinOpKind,
    UnOp,
};

impl RelationBinOpKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::Eq => "eq",
            Self::Le => "le",
            Self::Ge => "ge",
            Self::Ne => "ne",
            Self::Lt => "lt",
            Self::Gt => "gt",
        }
    }
}

impl Display for RelationBinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelationBinOpKind::Eq => write!(f, "="),
            RelationBinOpKind::Le => write!(f, "<="),
            RelationBinOpKind::Ge => write!(f, ">="),
            RelationBinOpKind::Ne => write!(f, "<>"),
            RelationBinOpKind::Lt => write!(f, "<"),
            RelationBinOpKind::Gt => write!(f, ">"),
        }
    }
}

impl IntBinOpKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
        }
    }
}

impl Display for IntBinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntBinOpKind::Add => write!(f, "+"),
            IntBinOpKind::Sub => write!(f, "-"),
            IntBinOpKind::Mul => write!(f, " * "),
            IntBinOpKind::Div => write!(f, "/"),
        }
    }
}

impl FloatBinOpKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::FAdd => "fadd",
            Self::FSub => "fsub",
            Self::FMul => "fmul",
            Self::FDiv => "fdiv",
        }
    }
}

impl Display for FloatBinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FloatBinOpKind::FAdd => write!(f, "+."),
            FloatBinOpKind::FSub => write!(f, "-."),
            FloatBinOpKind::FMul => write!(f, " *. "),
            FloatBinOpKind::FDiv => write!(f, "/."),
        }
    }
}

impl BinOp {
    pub fn name(self) -> &'static str {
        match self {
            Self::Relation(kind) => kind.name(),
            Self::Int(kind) => kind.name(),
            Self::Float(kind) => kind.name(),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Relation(kind) => write!(f, "{kind}"),
            BinOp::Int(kind) => write!(f, "{kind}"),
            BinOp::Float(kind) => write!(f, "{kind}"),
        }
    }
}

impl UnOp {
    pub fn name(self) -> &'static str {
        match self {
            Self::Neg => "neg",
            Self::Not => "not",
            Self::FNeg => "fneg",
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "not "),
            UnOp::FNeg => write!(f, "-."),
        }
    }
}

impl LitKind {
    pub fn discriminant_name(self) -> &'static str {
        match self {
            Self::Unit => "unit",
            Self::Bool(..) => "bool",
            Self::Int(..) => "int",
            Self::Float(..) => "float",
        }
    }
}

impl Display for LitKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitKind::Unit => write!(f, "()"),
            LitKind::Bool(b) => write!(f, "{b}"),
            LitKind::Int(i) => write!(f, "{i}"),
            LitKind::Float(f_bits) => write!(f, "{:.1}", f32::from_bits(*f_bits)),
        }
    }
}

impl Display for ExprKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Const(lit_kind) => write!(f, "{lit_kind}"),
            ExprKind::Unary(un_op, e) => write!(f, "{un_op}{e}"),
            ExprKind::Binary(bin_op, e1, e2) => write!(f, "({e1} {bin_op} {e2})"),
            ExprKind::If(e1, e2, e3) => write!(f, "if {e1} then ({e2}) else ({e3})"),
            ExprKind::Let(
                LetBinder {
                    pattern,
                    args,
                    value,
                },
                e,
            ) => {
                let pattern = match pattern {
                    Pattern::Var(v) => v.to_string(),
                    Pattern::Tuple(vs) => vs
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                };
                if args.is_empty() {
                    write!(f, "let {pattern} = {value} in\n{e}",)
                } else {
                    write!(
                        f,
                        "let rec {pattern} {args} = {value} in\n{e}",
                        args = args.iter().map(|arg| arg.0).collect::<Vec<_>>().join(" ")
                    )
                }
            }
            ExprKind::Then(e1, e2) => write!(f, "(({e1}); ({e2}))"),
            ExprKind::Var(v) => write!(f, "{v}"),
            ExprKind::App(e1, args) => write!(
                f,
                "({e1} {args})",
                args = args
                    .iter()
                    .map(|e| format!("({e})"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            ExprKind::Tuple(es) => write!(
                f,
                "({})",
                es.iter()
                    .map(|e| format!("({e})"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::ArrayMake(e1, e2) => write!(f, "Array.make ({e1}) ({e2})"),
            ExprKind::Get(e1, e2) => write!(f, "{e1}.({e2})"),
            ExprKind::Set(e1, e2) => write!(f, "{e1} <- ({e2})"),
        }
    }
}
