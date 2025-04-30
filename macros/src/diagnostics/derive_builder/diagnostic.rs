#![deny(unused_must_use)]

use crate::util::{is_doc_comment, span_err};

use crate::diagnostics::error::{throw_invalid_attr, DeriveError};
use crate::diagnostics::utils::{
    should_generate_arg, SetOnce, SpannedOption, SubdiagnosticKind, SubdiagnosticVariant,
};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::Token;
use syn::{parse_quote, spanned::Spanned, Attribute, Meta, Path};
use synstructure::{BindingInfo, Structure, VariantInfo};

use super::DiagnosticsDerive;

pub struct DiagnosticDerive;

impl<'s> DiagnosticsDerive<'s, ()> for DiagnosticDerive {
    type Builder = DiagnosticDeriveVariantBuilder;

    fn trait_name() -> &'static str {
        "Diagnostic"
    }

    fn new_builder(_structure: &Structure<'s>, span: proc_macro2::Span, _: ()) -> Self::Builder {
        DiagnosticDeriveVariantBuilder { span, slug: None }
    }
}

/// Tracks persistent information required for a specific variant when building up individual calls
/// to diagnostic methods for generated diagnostic derive - `Diagnostic` for
/// fatal/errors/warnings.
pub struct DiagnosticDeriveVariantBuilder {
    /// Span of the struct or the enum variant.
    pub span: Span,

    /// Slug is a mandatory part of the struct attribute as corresponds to the Fluent message that
    /// has the actual diagnostic message.
    pub slug: SpannedOption<Path>,
}

impl DiagnosticDeriveVariantBuilder {
    /// Generates calls to `code` and similar functions based on the attributes on the type or
    /// variant.
    pub(crate) fn preamble(&mut self, variant: &VariantInfo) -> TokenStream {
        let ast = variant.ast();
        let attrs = &ast.attrs;
        let preamble = attrs.iter().map(|attr| {
            self.generate_structure_code_for_attr(attr)
                .unwrap_or_else(|v| v.to_compile_error())
        });

        quote! {
            #(#preamble)*;
        }
    }

    /// Generates calls to `span_label` and similar functions based on the attributes on fields or
    /// calls to `arg` when no attributes are present.
    pub(crate) fn body(&mut self, variant: &VariantInfo) -> TokenStream {
        let mut body = quote! {};
        // Generate `arg` calls first..
        for binding in variant
            .bindings()
            .iter()
            .filter(|bi| should_generate_arg(bi.ast()))
        {
            body.extend(self.generate_field_code(binding));
        }
        // ..and then subdiagnostic additions.
        for binding in variant
            .bindings()
            .iter()
            .filter(|bi| !should_generate_arg(bi.ast()))
        {
            body.extend(self.generate_field_attrs_code(binding));
        }
        body
    }

    /// Parse a [`SubdiagnosticKind`] from an [`Attribute`].
    ///
    /// Some attributes aren't errors - like documentation comments - but also aren't
    /// subdiagnostics.
    fn parse_subdiag_attribute(
        &self,
        attr: &Attribute,
    ) -> Result<Option<(SubdiagnosticKind, Path)>, DeriveError> {
        let Some(subdiag) = SubdiagnosticVariant::from_attr(attr)? else {
            // Some attributes aren't errors - like documentation comments - but also aren't
            // subdiagnostics.
            return Ok(None);
        };

        let slug = subdiag.slug.unwrap_or_else(|| match subdiag.kind {
            SubdiagnosticKind::Label => parse_quote! { _subdiag::label },
            SubdiagnosticKind::Note => parse_quote! { _subdiag::note },
            SubdiagnosticKind::NoteOnce => parse_quote! { _subdiag::note_once },
            SubdiagnosticKind::Help => parse_quote! { _subdiag::help },
            SubdiagnosticKind::HelpOnce => parse_quote! { _subdiag::help_once },
            SubdiagnosticKind::Warn => parse_quote! { _subdiag::warn },
        });

        Ok(Some((subdiag.kind, slug)))
    }

    /// Establishes state in the `DiagnosticDeriveBuilder` resulting from the struct
    /// attributes like `#[diag(..)]`, such as the slug and error code. Generates
    /// diagnostic builder calls for setting error code and creating note/help messages.
    fn generate_structure_code_for_attr(
        &mut self,
        attr: &Attribute,
    ) -> Result<TokenStream, DeriveError> {
        // Always allow documentation comments.
        if is_doc_comment(attr) {
            return Ok(quote! {});
        }

        let name = attr.path().segments.last().unwrap().ident.to_string();
        let name = name.as_str();

        let mut first = true;

        if name == "diag" {
            attr.parse_nested_meta(|nested| {
                let path = &nested.path;

                if first && (nested.input.is_empty() || nested.input.peek(Token![,])) {
                    self.slug.set_once(path.clone(), path.span());
                    first = false;
                    return Ok(());
                }

                first = false;

                let Ok(nested) = nested.value() else {
                    span_err(
                        nested.input.span(),
                        "diagnostic slug must be the first argument",
                    )
                    .emit();
                    return Ok(());
                };

                span_err(path, "unknown argument")
                    .note("no parameters are valid after the slug")
                    .emit();

                // consume the buffer so we don't have syntax errors from syn
                let _ = nested.parse::<TokenStream>();
                Ok(())
            })?;
            return Ok(quote! {});
        }

        let Some((subdiag, slug)) = self.parse_subdiag_attribute(attr)? else {
            // Some attributes aren't errors - like documentation comments - but also aren't
            // subdiagnostics.
            return Ok(quote! {});
        };
        let fn_ident = format_ident!("{}", subdiag);
        match subdiag {
            SubdiagnosticKind::Note
            | SubdiagnosticKind::NoteOnce
            | SubdiagnosticKind::Help
            | SubdiagnosticKind::HelpOnce
            | SubdiagnosticKind::Warn => Ok(self.delegate_call(&fn_ident, slug, None)),
            SubdiagnosticKind::Label => {
                throw_invalid_attr!(attr, |diag| diag
                    .help("`#[label]` can only be applied to fields"));
            }
        }
    }

    fn generate_field_code(&mut self, binding_info: &BindingInfo<'_>) -> TokenStream {
        let field = binding_info.ast();
        let mut field_binding = binding_info.binding.clone();
        field_binding.set_span(field.ty.span());

        let ident = field.ident.as_ref().unwrap();
        let ident = format_ident!("{}", ident); // strip `r#` prefix, if present

        quote! {
            diag.arg(
                stringify!(#ident),
                #field_binding
            );
        }
    }

    /// Generates the necessary code for all attributes on a field.
    fn generate_field_attrs_code(&mut self, binding_info: &BindingInfo<'_>) -> TokenStream {
        let field = binding_info.ast();
        let field_binding = &binding_info.binding;

        field
            .attrs
            .iter()
            .map(move |attr| {
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
        let ident = &attr.path().segments.last().unwrap().ident;
        let name = ident.to_string();
        match (&attr.meta, name.as_str()) {
            // Don't need to do anything - by virtue of the attribute existing, the
            // `arg` call will not be generated.
            (Meta::Path(_), "skip_arg") => return Ok(quote! {}),
            (Meta::Path(_), "primary_span") => {
                return Ok(quote! {
                    diag.primary_span(#binding);
                });
            }
            (Meta::Path(_), "subdiagnostic") => {
                return Ok(quote! { diag.subdiagnostic(diag.dcx, #binding); });
            }
            _ => (),
        }

        let Some((subdiag, slug)) = self.parse_subdiag_attribute(attr)? else {
            // Some attributes aren't errors - like documentation comments - but also aren't
            // subdiagnostics.
            return Ok(quote! {});
        };
        let kind = format_ident!("{}", subdiag);
        match subdiag {
            SubdiagnosticKind::Label
            | SubdiagnosticKind::Note
            | SubdiagnosticKind::NoteOnce
            | SubdiagnosticKind::Help
            | SubdiagnosticKind::HelpOnce
            | SubdiagnosticKind::Warn => Ok(self.delegate_call(&kind, slug, Some(binding))),
        }
    }

    /// Delegates to `diag.$fn_name` calls with the given `fluent_identifier` and optional `arg`.
    fn delegate_call(
        &self,
        fn_name: &Ident,
        fluent_identifier: Path,
        arg: Option<TokenStream>,
    ) -> TokenStream {
        quote! {
            diag.#fn_name(crate::fluent_generated::#fluent_identifier, #arg);
        }
    }
}
