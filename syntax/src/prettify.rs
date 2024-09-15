use std::fmt::Display;

use crate::{BBinOpKind, BinOp, FBinOpKind, IBinOpKind, LitKind, UnOp};

impl BBinOpKind {
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

impl IBinOpKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
        }
    }
}

impl FBinOpKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::FAdd => "fadd",
            Self::FSub => "fsub",
            Self::FMul => "fmul",
            Self::FDiv => "fdiv",
        }
    }
}

impl BinOp {
    pub fn name(self) -> &'static str {
        match self {
            Self::BBinOp(kind) => kind.name(),
            Self::IBinOp(kind) => kind.name(),
            Self::FBinOp(kind) => kind.name(),
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
            LitKind::Float(f_bits) => write!(f, "{}", f32::from_bits(*f_bits)),
        }
    }
}
