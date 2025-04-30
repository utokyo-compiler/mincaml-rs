use proc_macro::Diagnostic;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, Error as SynError, Meta};

#[derive(Debug)]
pub(crate) enum DeriveError {
    SynError(SynError),
    ErrorHandled,
}

impl DeriveError {
    pub(crate) fn to_compile_error(&self) -> TokenStream {
        match self {
            DeriveError::SynError(e) => e.to_compile_error(),
            DeriveError::ErrorHandled => {
                // Return ! to avoid having to create a blank Diag to return when an
                // error has already been emitted to the compiler.
                quote! {
                    { unreachable!(); }
                }
            }
        }
    }
}

impl From<SynError> for DeriveError {
    fn from(e: SynError) -> Self {
        DeriveError::SynError(e)
    }
}

/// Helper function for use with `throw_*` macros - constraints `$f` to an `impl FnOnce`.
pub(crate) fn _throw_err(
    diag: Diagnostic,
    f: impl FnOnce(Diagnostic) -> Diagnostic,
) -> DeriveError {
    f(diag).emit();
    DeriveError::ErrorHandled
}

/// Helper function for printing `syn::Path` - doesn't handle arguments in paths and these are
/// unlikely to come up much in use of the macro.
fn path_to_string(path: &syn::Path) -> String {
    let mut out = String::new();
    for (i, segment) in path.segments.iter().enumerate() {
        if i > 0 || path.leading_colon.is_some() {
            out.push_str("::");
        }
        out.push_str(&segment.ident.to_string());
    }
    out
}

/// Emit a diagnostic on span `$span` with msg `$msg` (optionally performing additional decoration
/// using the `FnOnce` passed in `diag`) and return `Err(ErrorHandled)`.
///
/// For methods that return a `Result<_, DiagnosticDeriveError>`:
macro_rules! throw_span_err {
    ($span:expr, $msg:expr) => {{
        throw_span_err!($span, $msg, |diag| diag)
    }};
    ($span:expr, $msg:expr, $f:expr) => {{
        let diag = span_err($span, $msg);
        return Err($crate::diagnostics::error::_throw_err(diag, $f));
    }};
}

pub(crate) use throw_span_err;

/// Returns an error diagnostic for an invalid attribute.
pub(crate) fn invalid_attr(attr: &Attribute) -> Diagnostic {
    let path = path_to_string(attr.path());
    match attr.meta {
        Meta::Path(_) => span_err(attr, format!("`#[{path}]` is not a valid attribute")),
        Meta::NameValue(_) => span_err(attr, format!("`#[{path} = ...]` is not a valid attribute")),
        Meta::List(_) => span_err(attr, format!("`#[{path}(...)]` is not a valid attribute")),
    }
}

/// Emit an error diagnostic for an invalid attribute (optionally performing additional decoration
/// using the `FnOnce` passed in `diag`) and return `Err(ErrorHandled)`.
///
/// For methods that return a `Result<_, DiagnosticDeriveError>`:
macro_rules! throw_invalid_attr {
    ($attr:expr) => {{
        throw_invalid_attr!($attr, |diag| diag)
    }};
    ($attr:expr, $f:expr) => {{
        let diag = $crate::diagnostics::error::invalid_attr($attr);
        return Err($crate::diagnostics::error::_throw_err(diag, $f));
    }};
}

pub(crate) use throw_invalid_attr;

use crate::util::span_err;
