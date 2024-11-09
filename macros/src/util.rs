use proc_macro::{Diagnostic, Level};
use syn::{spanned::Spanned, Attribute};

pub fn is_doc_comment(attr: &Attribute) -> bool {
    attr.path().segments.last().unwrap().ident == "doc"
}

/// Returns an error diagnostic on span `span` with msg `msg`.
#[must_use]
pub fn span_err<T: Into<String>>(span: impl Spanned, msg: T) -> Diagnostic {
    Diagnostic::spanned(span.span().unwrap(), Level::Error, msg)
}
