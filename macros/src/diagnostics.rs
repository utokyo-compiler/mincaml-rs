use std::cell::RefCell;

use derive_builder::{diagnostic::DiagnosticDerive, subdiagnostic::SubDiagnostic};
use error::DeriveError;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use synstructure::Structure;
use utils::SetOnce;

use crate::util::span_err;

mod derive_builder;
mod error;
mod utils;

/// Implements `#[derive(Diagnostic)]`, which allows for errors to be specified as a struct,
/// independent from the actual diagnostics emitting code.
///
/// ```ignore (rust)
/// # use sourcemap::Span;
/// # use ast::Ident;
/// # use ty::Ty;
/// #[derive(Diagnostic)]
/// #[diag(borrowck_move_out_of_borrow)]
/// pub struct MoveOutOfBorrowError<'ctx> {
///     pub name: Ident<'ctx>,
///     pub ty: Ty<'ctx>,
///     #[primary_span]
///     #[label]
///     pub span: Span,
///     #[label(first_borrow_label)]
///     pub first_borrow_span: Span,
///     #[suggestion(code = "{name}.clone()")]
///     pub clone_sugg: Option<Span>
/// }
/// ```
///
/// ```fluent
/// move_out_of_borrow = cannot move out of {$name} because it is borrowed
///     .label = cannot move out of borrow
///     .first_borrow_label = `{$ty}` first borrowed here
///     .suggestion = consider cloning here
/// ```
///
/// Then, later, to emit the error:
///
/// ```ignore (rust)
/// gcx.dcx().emit_err(MoveOutOfBorrowError {
///     name,
///     ty,
///     span,
///     first_borrow_span,
///     clone_sugg: Some(suggestion),
/// });
/// ```
///
/// See rustc dev guide for more examples on using the `#[derive(Diagnostic)]`:
/// <https://rustc-dev-guide.rust-lang.org/diagnostics/diagnostic-structs.html>
///
/// See also the `rustc` counterpart: `rustc_macros::diagnostics::diagnostic_derive`.
pub(super) fn diagnostic_macro_derive(mut structure: Structure) -> TokenStream {
    let slugs = RefCell::new(Vec::new());
    let implementation = derive_builder::each_variant(
        DiagnosticDerive,
        (),
        &mut structure,
        |mut builder, variant| {
            let preamble = builder.preamble(variant);
            let body = builder.body(variant);

            let init = match builder.slug.value_ref() {
                None => {
                    span_err(builder.span, "diagnostic slug not specified")
                        .help(
                            "specify the slug as the first argument to the `#[diag(...)]` \
                            attribute, such as `#[diag(hir_analysis_example_error)]`",
                        )
                        .emit();
                    return DeriveError::ErrorHandled.to_compile_error();
                }
                Some(slug) => {
                    if let Some(Mismatch {
                        slug_name,
                        crate_name,
                    }) = Mismatch::check(slug)
                    {
                        span_err(slug.span(), "diagnostic slug and crate name do not match")
                            .note(format!(
                                "slug is `{slug_name}` but the crate name is `{crate_name}`"
                            ))
                            .help(format!("expected a slug starting with `{crate_name}_...`"))
                            .emit();
                        return DeriveError::ErrorHandled.to_compile_error();
                    } else {
                        slugs.borrow_mut().push(slug.clone());
                        quote! {
                            let mut diag = ::errors::Diag::new(
                                dcx,
                                level,
                                crate::fluent_generated::#slug
                            );
                        }
                    }
                }
            };

            quote! {
                #init
                #preamble
                #body
                diag
            }
        },
    );

    // A lifetime of `'a` causes conflicts, but `_sess` is fine.
    structure.gen_impl(quote! {
        gen impl ::errors::Diagnostic for @Self
        {
            fn into_diag<'_sess>(
                self,
                dcx: &'_sess ::errors::DiagContext<'_sess>,
                level: ::errors::Level
            ) -> ::errors::Diag<'_sess> {
                #implementation
            }
        }
    })
}

struct Mismatch {
    slug_name: String,
    crate_name: String,
}

impl Mismatch {
    /// Checks whether the slug starts with the crate name it's in.
    fn check(slug: &syn::Path) -> Option<Mismatch> {
        // If this is missing we're probably in a test, so bail.
        let crate_name = std::env::var("CARGO_CRATE_NAME").ok()?;

        let slug_name = slug.segments.first()?.ident.to_string();
        if !slug_name.starts_with(&crate_name) {
            Some(Mismatch {
                slug_name,
                crate_name,
            })
        } else {
            None
        }
    }
}

/// Implements `#[derive(Subdiagnostic)]`, which allows for labels, notes, helps and
/// suggestions to be specified as a structs or enums, independent from the actual diagnostics
/// emitting code or diagnostic derives.
///
/// ```ignore (rust)
/// #[derive(Subdiagnostic)]
/// pub enum ExpectedIdentifierLabel<'tcx> {
///     #[label(expected_identifier)]
///     WithoutFound {
///         #[primary_span]
///         span: Span,
///     }
///     #[label(expected_identifier_found)]
///     WithFound {
///         #[primary_span]
///         span: Span,
///         found: String,
///     }
/// }
///
/// #[derive(Subdiagnostic)]
/// pub struct RawIdentifierSuggestion<'tcx> {
///     #[primary_span]
///     span: Span,
///     ident: Ident,
/// }
/// ```
///
/// ```fluent
/// parser_expected_identifier = expected identifier
///
/// parser_expected_identifier_found = expected identifier, found {$found}
///
/// parser_raw_identifier = escape `{$ident}` to use it as an identifier
/// ```
///
/// Then, later, to add the subdiagnostic:
///
/// ```ignore (rust)
/// diag.subdiagnostic(ExpectedIdentifierLabel::WithoutFound { span });
///
/// diag.subdiagnostic(RawIdentifierSuggestion { span, ident });
/// ```
///
/// See also the `rustc` counterpart: `rustc_macros::diagnostics::subdiagnostic_derive`.
pub(super) fn subdiagnostic_derive(mut structure: Structure) -> TokenStream {
    let diag = format_ident!("diag");
    let diag = &diag;
    let implementation = derive_builder::each_variant(
        SubDiagnostic,
        diag,
        &mut structure,
        |mut builder, variant| {
            let kind_slugs = match builder.identify_kind(variant) {
                Ok(kind_slugs) => kind_slugs,
                Err(err) => return err.to_compile_error(),
            };

            let attrs_code = builder.attrs_code(variant);
            if kind_slugs.is_empty() && !builder.has_subdiagnostic {
                if builder.is_enum {
                    // It's okay for a variant to not be a subdiagnostic at all..
                    return quote! {};
                } else {
                    // ..but structs should always be _something_.
                    span_err(
                        variant.ast().ident.span(),
                        "subdiagnostic kind not specified",
                    )
                    .emit();
                    return DeriveError::ErrorHandled.to_compile_error();
                }
            };

            let calls = builder.calls(kind_slugs);

            let plain_args = builder.plain_args(variant);

            quote! {
                #attrs_code
                #plain_args
                #calls
            }
        },
    );

    structure.gen_impl(quote! {
        gen impl errors::Subdiagnostic for @Self {
            fn add_to_diag(
                self,
                #diag: &mut errors::Diag<'_>,
            )
            {
                #implementation
            }
        }
    })
}
