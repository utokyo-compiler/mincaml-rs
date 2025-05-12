#![deny(unused_must_use)]

use crate::util::{is_doc_comment, span_err};

use crate::diagnostics::error::{invalid_attr, throw_span_err, DeriveError};
use crate::diagnostics::utils::{
    should_generate_arg, SetOnce, SpannedOption, SubdiagnosticKind, SubdiagnosticVariant,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Attribute, Meta, Path};
use synstructure::{BindingInfo, Structure, VariantInfo};

use super::DiagnosticsDerive;

pub struct SubDiagnostic;

impl<'diag, 's> DiagnosticsDerive<'s, &'diag syn::Ident> for SubDiagnostic {
    type Builder = SubdiagnosticDeriveVariantBuilder<'diag>;

    fn trait_name() -> &'static str {
        "Subdiagnostic"
    }

    fn new_builder(
        structure: &Structure<'s>,
        span: proc_macro2::Span,
        diag: &'diag syn::Ident,
    ) -> Self::Builder {
        let is_enum = matches!(structure.ast().data, syn::Data::Enum(..));
        SubdiagnosticDeriveVariantBuilder {
            diag,
            span,
            span_field: None,
            has_subdiagnostic: false,
            is_enum,
        }
    }
}

/// Tracks persistent information required for building up the call to add to the diagnostic
/// for the final generated method. This is a separate struct to [`SubdiagnosticDerive`]
/// only to be able to destructure and split `self.builder` and the `self.structure` up to avoid a
/// double mut borrow later on.
pub struct SubdiagnosticDeriveVariantBuilder<'diag> {
    /// The identifier to use for the generated `Diag` instance.
    diag: &'diag syn::Ident,

    /// Span for the entire type.
    span: proc_macro2::Span,

    /// Expression for the binding to the `#[primary_span]` field.
    span_field: SpannedOption<TokenStream>,

    /// Set to true when a `#[subdiagnostic]` field is encountered, used to suppress the error
    /// emitted when no subdiagnostic kinds are specified on the variant itself.
    pub has_subdiagnostic: bool,

    /// Set to true when this variant is an enum variant rather than just the body of a struct.
    pub is_enum: bool,
}

impl<'diag> SubdiagnosticDeriveVariantBuilder<'diag> {
    pub fn identify_kind(
        &self,
        variant: &VariantInfo,
    ) -> Result<Vec<(SubdiagnosticKind, Path, bool)>, DeriveError> {
        let mut kind_slugs = vec![];

        for attr in variant.ast().attrs {
            let Some(SubdiagnosticVariant {
                kind,
                slug,
                no_span,
            }) = SubdiagnosticVariant::from_attr(attr)?
            else {
                // Some attributes aren't errors - like documentation comments - but also aren't
                // subdiagnostics.
                continue;
            };

            let Some(slug) = slug else {
                let name = attr.path().segments.last().unwrap().ident.to_string();
                let name = name.as_str();

                throw_span_err!(
                    attr.span(),
                    format!(
                        "diagnostic slug must be first argument of a `#[{name}(...)]` attribute"
                    )
                );
            };

            kind_slugs.push((kind, slug, no_span));
        }

        Ok(kind_slugs)
    }

    /// Generates the code for a field with no attributes.
    fn generate_field_arg(&mut self, binding_info: &BindingInfo<'_>) -> TokenStream {
        let diag = &self.diag;

        let field = binding_info.ast();
        let mut field_binding = binding_info.binding.clone();
        field_binding.set_span(field.ty.span());

        let ident = field.ident.as_ref().unwrap();
        let ident = format_ident!("{}", ident); // strip `r#` prefix, if present

        quote! {
            #diag.arg(
                stringify!(#ident),
                #field_binding
            );
        }
    }

    /// Generates the necessary code for all attributes on a field.
    fn generate_field_attrs_code(&mut self, binding_info: &BindingInfo<'_>) -> TokenStream {
        let field = binding_info.ast();
        let field_binding = &binding_info.binding;
        assert!(
            !field.attrs.is_empty(),
            "field without attributes generating attr code"
        );

        field
            .attrs
            .iter()
            .map(|attr| {
                // Always allow documentation comments.
                if is_doc_comment(attr) {
                    return quote! {};
                }

                let binding = quote_spanned! {field.ty.span()=> #field_binding.clone() };

                self.generate_inner_field_code(attr, binding)
                    .unwrap_or_else(|v| v.to_compile_error())
            })
            .collect()
    }

    fn generate_inner_field_code(
        &mut self,
        attr: &Attribute,
        binding: TokenStream,
    ) -> Result<TokenStream, DeriveError> {
        let span = attr.span();
        let ident = &attr.path().segments.last().unwrap().ident;
        let name = ident.to_string();

        match (&attr.meta, name.as_str()) {
            // Don't need to do anything - by virtue of the attribute existing, the
            // `arg` call will not be generated.
            (Meta::Path(_), "skip_arg") => Ok(quote! {}),
            (Meta::Path(_), "primary_span") => {
                let binding = binding.clone();

                self.span_field.set_once(binding, span);

                Ok(quote! {})
            }
            (Meta::Path(_), "subdiagnostic") => {
                let diag = self.diag;
                self.has_subdiagnostic = true;
                Ok(quote! { #binding.add_to_diag(#diag); })
            }
            _ => {
                invalid_attr(attr)
                    .help("only `primary_span`, `subdiagnostic` and `skip_arg` are valid field attributes")
                    .emit();

                Ok(quote! {})
            }
        }
    }

    pub fn attrs_code(&mut self, variant: &VariantInfo<'_>) -> TokenStream {
        variant
            .bindings()
            .iter()
            .filter(|binding| !should_generate_arg(binding.ast()))
            .map(|binding| self.generate_field_attrs_code(binding))
            .collect()
    }

    pub fn plain_args(&mut self, variant: &VariantInfo) -> TokenStream {
        variant
            .bindings()
            .iter()
            .filter(|binding| should_generate_arg(binding.ast()))
            .map(|binding| self.generate_field_arg(binding))
            .collect()
    }

    pub fn calls(&mut self, kind_slugs: Vec<(SubdiagnosticKind, Path, bool)>) -> TokenStream {
        let diag = &self.diag;
        let span_field = self.span_field.value_ref();
        let mut calls = TokenStream::new();
        for (kind, slug, no_span) in kind_slugs {
            let message = format_ident!("__message");
            calls.extend(quote! { let #message = crate::fluent_generated::#slug; });

            let fn_name = format_ident!("{}", kind);
            let call = match kind {
                SubdiagnosticKind::Label => {
                    if let Some(span) = span_field {
                        quote! { #diag.may_add_label(#message, #span); }
                    } else {
                        span_err(self.span, "label without `#[primary_span]` field").emit();
                        quote! { unreachable!(); }
                    }
                }
                _ => {
                    if let Some(span) = span_field
                        && !no_span
                    {
                        quote! { #diag.#fn_name(#message, #span); }
                    } else {
                        quote! { #diag.#fn_name(#message, errors::AlwaysShow); }
                    }
                }
            };

            calls.extend(call);
        }
        calls
    }
}
