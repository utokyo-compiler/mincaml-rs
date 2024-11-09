use std::cell::RefCell;

use error::DeriveError;
use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use synstructure::Structure;
use utils::SetOnce;

use crate::util::span_err;

mod builder;
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
pub fn diagnostic_macro_derive(mut structure: Structure) -> TokenStream {
    let slugs = RefCell::new(Vec::new());
    let implementation = builder::each_variant(&mut structure, |mut builder, variant| {
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

        let formatting_init = &builder.formatting_init;
        quote! {
            #init
            #formatting_init
            #preamble
            #body
            diag
        }
    });

    // A lifetime of `'a` causes conflicts, but `_sess` is fine.
    let mut imp = structure.gen_impl(quote! {
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
    });
    for test in slugs.borrow().iter().map(|s| generate_test(s, &structure)) {
        imp.extend(test);
    }

    imp
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

/// Generates a `#[test]` that verifies that all referenced variables
/// exist on this structure.
fn generate_test(slug: &syn::Path, structure: &Structure<'_>) -> TokenStream {
    // FIXME: We can't identify variables in a subdiagnostic
    for field in structure
        .variants()
        .iter()
        .flat_map(|v| v.ast().fields.iter())
    {
        for attr_name in field.attrs.iter().filter_map(|at| at.path().get_ident()) {
            if attr_name == "subdiagnostic" {
                return quote!();
            }
        }
    }
    use std::sync::atomic::{AtomicUsize, Ordering};
    // We need to make sure that the same diagnostic slug can be used multiple times without
    // causing an error, so just have a global counter here.
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let slug = slug.get_ident().unwrap();
    let ident = quote::format_ident!("verify_{slug}_{}", COUNTER.fetch_add(1, Ordering::Relaxed));
    let ref_slug = quote::format_ident!("{slug}_refs");
    let struct_name = &structure.ast().ident;
    let variables: Vec<_> = structure
        .variants()
        .iter()
        .flat_map(|v| {
            v.ast()
                .fields
                .iter()
                .filter_map(|f| f.ident.as_ref().map(|i| i.to_string()))
        })
        .collect();
    // tidy errors on `#[test]` outside of test files, so we use `#[test ]` to work around this
    quote! {
        #[cfg(test)]
        #[test ]
        fn #ident() {
            let variables = [#(#variables),*];
            for vref in crate::fluent_generated::#ref_slug {
                assert!(variables.contains(vref), "{}: variable `{vref}` not found ({})", stringify!(#struct_name), stringify!(#slug));
            }
        }
    }
}
