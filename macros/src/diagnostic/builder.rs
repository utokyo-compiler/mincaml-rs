#![deny(unused_must_use)]

use crate::util::{is_doc_comment, span_err};

use super::error::{throw_invalid_attr, DeriveError};
use super::utils::{
    build_field_mapping, should_generate_arg, FieldMap, HasFieldMap, SetOnce, SpannedOption,
    SubdiagnosticKind, SubdiagnosticVariant,
};

use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::Token;
use syn::{parse_quote, spanned::Spanned, Attribute, Meta, Path};
use synstructure::{BindingInfo, Structure, VariantInfo};

/// Tracks persistent information required for a specific variant when building up individual calls
/// to diagnostic methods for generated diagnostic derives - both `Diagnostic` for
/// fatal/errors/warnings and `LintDiagnostic` for lints.
pub(crate) struct DiagnosticDeriveVariantBuilder {
    /// Initialization of format strings for code suggestions.
    pub formatting_init: TokenStream,

    /// Span of the struct or the enum variant.
    pub span: Span,

    /// Store a map of field name to its corresponding field. This is built on construction of the
    /// derive builder.
    pub field_map: FieldMap,

    /// Slug is a mandatory part of the struct attribute as corresponds to the Fluent message that
    /// has the actual diagnostic message.
    pub slug: SpannedOption<Path>,
}

impl HasFieldMap for DiagnosticDeriveVariantBuilder {
    fn get_field_binding(&self, field: &str) -> Option<&TokenStream> {
        self.field_map.get(field)
    }
}

/// Call `f` for the struct or for each variant of the enum, returning a `TokenStream` with the
/// tokens from `f` wrapped in an `match` expression. Emits errors for use of derive on unions
/// or attributes on the type itself when input is an enum.
pub(crate) fn each_variant<'s, F>(structure: &mut Structure<'s>, f: F) -> TokenStream
where
    F: for<'v> Fn(DiagnosticDeriveVariantBuilder, &VariantInfo<'v>) -> TokenStream,
{
    let ast = structure.ast();
    let span = ast.span();
    match ast.data {
        syn::Data::Struct(..) | syn::Data::Enum(..) => (),
        syn::Data::Union(..) => {
            span_err(
                span,
                "diagnostic derives can only be used on structs and enums",
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
        let builder = DiagnosticDeriveVariantBuilder {
            span,
            field_map: build_field_mapping(variant),
            formatting_init: TokenStream::new(),
            slug: None,
        };
        f(builder, variant)
    });

    quote! {
        match self {
            #variants
        }
    }
}

impl DiagnosticDeriveVariantBuilder {
    /// Generates calls to `code` and similar functions based on the attributes on the type or
    /// variant.
    pub(crate) fn preamble(&mut self, variant: &VariantInfo<'_>) -> TokenStream {
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
    pub(crate) fn body(&mut self, variant: &VariantInfo<'_>) -> TokenStream {
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

    /// Parse a `SubdiagnosticKind` from an `Attribute`.
    ///
    /// Some attributes aren't errors - like documentation comments - but also aren't
    /// subdiagnostics.
    fn parse_subdiag_attribute(
        &self,
        attr: &Attribute,
    ) -> Result<Option<(SubdiagnosticKind, Path)>, DeriveError> {
        let Some(subdiag) = SubdiagnosticVariant::from_attr(attr, self)? else {
            // Some attributes aren't errors - like documentation comments - but also aren't
            // subdiagnostics.
            return Ok(None);
        };

        if let SubdiagnosticKind::MultipartSuggestion { .. } = subdiag.kind {
            throw_invalid_attr!(attr, |diag| diag
                .help("consider creating a `Subdiagnostic` instead"));
        }

        let slug = subdiag.slug.unwrap_or_else(|| match subdiag.kind {
            SubdiagnosticKind::Label => parse_quote! { _subdiag::label },
            SubdiagnosticKind::Note => parse_quote! { _subdiag::note },
            SubdiagnosticKind::NoteOnce => parse_quote! { _subdiag::note_once },
            SubdiagnosticKind::Help => parse_quote! { _subdiag::help },
            SubdiagnosticKind::HelpOnce => parse_quote! { _subdiag::help_once },
            SubdiagnosticKind::Warn => parse_quote! { _subdiag::warn },
            SubdiagnosticKind::Suggestion { .. } => parse_quote! { _subdiag::suggestion },
            SubdiagnosticKind::MultipartSuggestion { .. } => unreachable!(),
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
            SubdiagnosticKind::Label | SubdiagnosticKind::Suggestion { .. } => {
                throw_invalid_attr!(attr, |diag| diag
                    .help("`#[label]` and `#[suggestion]` can only be applied to fields"));
            }
            SubdiagnosticKind::MultipartSuggestion { .. } => unreachable!(),
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
        let fn_ident = format_ident!("{}", subdiag);
        match subdiag {
            SubdiagnosticKind::Label
            | SubdiagnosticKind::Note
            | SubdiagnosticKind::NoteOnce
            | SubdiagnosticKind::Help
            | SubdiagnosticKind::HelpOnce
            | SubdiagnosticKind::Warn => Ok(self.delegate_call(&fn_ident, slug, Some(binding))),
            SubdiagnosticKind::Suggestion { .. } => {
                span_err(attr, "suggestion attributes are not implemented yet").emit();
                Ok(quote! {})
            }
            SubdiagnosticKind::MultipartSuggestion { .. } => unreachable!(),
        }
    }

    /// Delegates to `diag.$kind` calls with the given `fluent_identifier` and optional `arg`.
    fn delegate_call(
        &self,
        kind: &Ident,
        fluent_identifier: Path,
        arg: Option<TokenStream>,
    ) -> TokenStream {
        quote! {
            diag.#kind(crate::fluent_generated::#fluent_identifier, #arg);
        }
    }
}
