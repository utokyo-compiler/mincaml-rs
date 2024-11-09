use std::{
    borrow::Cow,
    fmt,
    path::{Path, PathBuf},
};

use crate::{DiagArgValue, IntoDiagArg};

pub struct DiagArgFromDisplay<'a>(pub &'a dyn fmt::Display);

impl IntoDiagArg for DiagArgFromDisplay<'_> {
    fn into_diag_arg(self) -> DiagArgValue {
        self.0.to_string().into_diag_arg()
    }
}

impl<'a> From<&'a dyn fmt::Display> for DiagArgFromDisplay<'a> {
    fn from(t: &'a dyn fmt::Display) -> Self {
        DiagArgFromDisplay(t)
    }
}

impl<'a, T: fmt::Display> From<&'a T> for DiagArgFromDisplay<'a> {
    fn from(t: &'a T) -> Self {
        DiagArgFromDisplay(t)
    }
}

impl<'a, T: Clone + IntoDiagArg> IntoDiagArg for &'a T {
    fn into_diag_arg(self) -> DiagArgValue {
        self.clone().into_diag_arg()
    }
}

#[macro_export]
macro_rules! into_diag_arg_using_display {
    ($( $ty:ty ),+ $(,)?) => {
        $(
            impl $crate::IntoDiagArg for $ty {
                fn into_diag_arg(self) -> $crate::DiagArgValue {
                    self.to_string().into_diag_arg()
                }
            }
        )+
    }
}

macro_rules! into_diag_arg_for_number {
    ($( $ty:ty ),+ $(,)?) => {
        $(
            impl IntoDiagArg for $ty {
                fn into_diag_arg(self) -> DiagArgValue {
                    // Convert to a string if it won't fit into `Number`.
                    if let Ok(n) = TryInto::<i32>::try_into(self) {
                        DiagArgValue::Number(n)
                    } else {
                        self.to_string().into_diag_arg()
                    }
                }
            }
        )+
    };
    (!refutables: $( $ty:ty ),+ $(,)?) => {
        $(
            impl IntoDiagArg for $ty {
                fn into_diag_arg(self) -> DiagArgValue {
                    DiagArgValue::Number(self as i32)
                }
            }
        )+
    }
}

into_diag_arg_using_display!(
    std::io::Error,
    Box<dyn std::error::Error>,
    std::num::NonZero<u32>,
);

into_diag_arg_for_number!(u32, i64, u64, i128, u128, isize, usize);
into_diag_arg_for_number!(!refutables: i8, u8, i16, u16, i32);

impl IntoDiagArg for bool {
    fn into_diag_arg(self) -> DiagArgValue {
        if self {
            DiagArgValue::Str(Cow::Borrowed("true"))
        } else {
            DiagArgValue::Str(Cow::Borrowed("false"))
        }
    }
}

impl IntoDiagArg for char {
    fn into_diag_arg(self) -> DiagArgValue {
        DiagArgValue::Str(Cow::Owned(format!("{self:?}")))
    }
}

impl<'a> IntoDiagArg for &'a str {
    fn into_diag_arg(self) -> DiagArgValue {
        self.to_string().into_diag_arg()
    }
}

impl IntoDiagArg for String {
    fn into_diag_arg(self) -> DiagArgValue {
        DiagArgValue::Str(Cow::Owned(self))
    }
}

impl<'a> IntoDiagArg for Cow<'a, str> {
    fn into_diag_arg(self) -> DiagArgValue {
        DiagArgValue::Str(Cow::Owned(self.into_owned()))
    }
}

impl<'a> IntoDiagArg for &'a Path {
    fn into_diag_arg(self) -> DiagArgValue {
        DiagArgValue::Str(Cow::Owned(self.display().to_string()))
    }
}

impl IntoDiagArg for PathBuf {
    fn into_diag_arg(self) -> DiagArgValue {
        DiagArgValue::Str(Cow::Owned(self.display().to_string()))
    }
}
