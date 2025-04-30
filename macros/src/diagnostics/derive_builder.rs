use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use synstructure::{Structure, VariantInfo};

use crate::util::{is_doc_comment, span_err};

pub mod diagnostic;
pub mod subdiagnostic;

/// Abstracts over the `#[derive(Diagnostic)]` and `#[derive(Subdiagnostic)]` derives.
///
/// This is an abstraction that exposes only the code generation overview and hides the rest.
pub trait DiagnosticsDerive<'s, Context: Copy> {
    type Builder;

    /// The name of the trait that this derive implements.
    ///
    /// This is used for error messages.
    fn trait_name() -> &'static str;

    /// Create a new builder for the given structure and variant.
    ///
    /// It can receive a `Context` that lives longer than the variant processing.
    /// `Context` is required to be [`Copy`] so that it can be passed to [`FnMut`] closures.
    fn new_builder(
        structure: &Structure<'s>,
        span: proc_macro2::Span,
        context: Context,
    ) -> Self::Builder;
}

/// Call `f` for the struct or for each variant of the enum, returning a `TokenStream` with the
/// tokens from `f` wrapped in an `match` expression. Emits errors for use of derive on unions
/// or attributes on the type itself when input is an enum.
pub(crate) fn each_variant<'s, Derive: DiagnosticsDerive<'s, Context>, Context: Copy, F>(
    _derive: Derive,
    context: Context,
    structure: &mut Structure<'s>,
    f: F,
) -> TokenStream
where
    F: for<'v> Fn(Derive::Builder, &VariantInfo<'v>) -> TokenStream,
{
    let ast = structure.ast();
    let span = ast.span();
    match ast.data {
        syn::Data::Struct(..) | syn::Data::Enum(..) => (),
        syn::Data::Union(..) => {
            span_err(
                span,
                format!(
                    "#[derive({})] can only be used on structs and enums",
                    Derive::trait_name()
                ),
            )
            .emit();
        }
    }

    if matches!(ast.data, syn::Data::Enum(..)) {
        for attr in &ast.attrs {
            if !is_doc_comment(attr) {
                span_err(
                    attr.span(),
                    "unsupported type attribute for diagnostic derive enum",
                )
                .emit();
            }
        }
    }

    structure.bind_with(|_| synstructure::BindStyle::Move);
    let variants = structure.each_variant(|variant| {
        let span = match structure.ast().data {
            syn::Data::Struct(..) => span,
            // There isn't a good way to get the span of the variant, so the variant's
            // name will need to do.
            _ => variant.ast().ident.span(),
        };
        let builder = Derive::new_builder(structure, span, context);
        f(builder, variant)
    });

    quote! {
        match self {
            #variants
        }
    }
}
