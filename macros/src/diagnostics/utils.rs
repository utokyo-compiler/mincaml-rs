use super::error::{throw_invalid_attr, DeriveError};
use proc_macro2::{Span, TokenStream};
use std::fmt;
use syn::{spanned::Spanned, Attribute, Field, Meta};
use syn::{Path, Token};

use crate::util::{is_doc_comment, span_err};

/// Small helper trait for abstracting over `Option` fields that contain a value and a `Span`
/// for error reporting if they are set more than once.
pub(crate) trait SetOnce<T> {
    fn set_once(&mut self, value: T, span: Span);

    #[allow(dead_code)]
    fn value(self) -> Option<T>;
    fn value_ref(&self) -> Option<&T>;
}

/// An [`Option<T>`] that keeps track of the span that caused it to be set; used with [`SetOnce`].
pub(super) type SpannedOption<T> = Option<(T, Span)>;

impl<T> SetOnce<T> for SpannedOption<T> {
    fn set_once(&mut self, value: T, span: Span) {
        match self {
            None => {
                *self = Some((value, span));
            }
            Some((_, prev_span)) => {
                span_err(span, "specified multiple times")
                    .span_note(prev_span.unwrap(), "previously specified here")
                    .emit();
            }
        }
    }

    fn value(self) -> Option<T> {
        self.map(|(v, _)| v)
    }

    fn value_ref(&self) -> Option<&T> {
        self.as_ref().map(|(v, _)| v)
    }
}

/// Types of subdiagnostics that can be created using attributes
#[derive(Clone)]
pub(super) enum SubdiagnosticKind {
    /// `#[label(...)]`
    Label,
    /// `#[note(...)]`
    Note,
    /// `#[note_once(...)]`
    NoteOnce,
    /// `#[help(...)]`
    Help,
    /// `#[help_once(...)]`
    HelpOnce,
    /// `#[warning(...)]`
    Warn,
}

pub(super) struct SubdiagnosticVariant {
    pub(super) kind: SubdiagnosticKind,
    pub(super) slug: Option<Path>,
    pub(super) no_span: bool,
}

impl SubdiagnosticVariant {
    /// Constructs a [`SubdiagnosticVariant`] from a field or type attribute such as `#[note]`,
    /// `#[error(parser::add_paren, no_span)]` or `#[suggestion(code = "...")]`. Returns the
    /// `SubdiagnosticKind` and the diagnostic slug, if specified.
    pub(super) fn from_attr(attr: &Attribute) -> Result<Option<SubdiagnosticVariant>, DeriveError> {
        // Always allow documentation comments.
        if is_doc_comment(attr) {
            return Ok(None);
        }

        let name = attr.path().segments.last().unwrap().ident.to_string();
        let name = name.as_str();

        let kind = match name {
            "label" => SubdiagnosticKind::Label,
            "note" => SubdiagnosticKind::Note,
            "note_once" => SubdiagnosticKind::NoteOnce,
            "help" => SubdiagnosticKind::Help,
            "help_once" => SubdiagnosticKind::HelpOnce,
            "warning" => SubdiagnosticKind::Warn,
            _ => {
                throw_invalid_attr!(attr);
            }
        };

        let list = match &attr.meta {
            Meta::List(list) => {
                // An attribute with properties, such as `#[suggestion(code = "...")]` or
                // `#[error(some::slug)]`
                list
            }
            Meta::Path(_) => {
                // An attribute without a slug or other properties, such as `#[note]` - return
                // without further processing.
                //
                // Only allow this if there are no mandatory properties, such as `code = "..."` in
                // `#[suggestion(...)]`
                match kind {
                    SubdiagnosticKind::Label
                    | SubdiagnosticKind::Note
                    | SubdiagnosticKind::NoteOnce
                    | SubdiagnosticKind::Help
                    | SubdiagnosticKind::HelpOnce
                    | SubdiagnosticKind::Warn => {
                        return Ok(Some(SubdiagnosticVariant {
                            kind,
                            slug: None,
                            no_span: false,
                        }));
                    }
                }
            }
            _ => {
                throw_invalid_attr!(attr)
            }
        };

        let mut first = true;
        let mut slug = None;
        let mut no_span = false;

        list.parse_nested_meta(|nested| {
            if nested.input.is_empty() || nested.input.peek(Token![,]) {
                if first {
                    slug = Some(nested.path);
                } else if nested.path.is_ident("no_span") {
                    no_span = true;
                } else {
                    span_err(
                        nested.input.span(),
                        "a diagnostic slug must be the first argument to the attribute",
                    )
                    .emit();
                }

                first = false;
                return Ok(());
            }

            span_err(
                nested.path.span(),
                "only `no_span` is a valid nested attribute",
            )
            .emit();

            // Consume the rest of the input to avoid spamming errors
            let _ = nested.input.parse::<TokenStream>();

            Ok(())
        })?;

        Ok(Some(SubdiagnosticVariant {
            kind,
            slug,
            no_span,
        }))
    }
}

impl quote::IdentFragment for SubdiagnosticKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SubdiagnosticKind::Label => write!(f, "label"),
            SubdiagnosticKind::Note => write!(f, "note"),
            SubdiagnosticKind::NoteOnce => write!(f, "note_once"),
            SubdiagnosticKind::Help => write!(f, "help"),
            SubdiagnosticKind::HelpOnce => write!(f, "help_once"),
            SubdiagnosticKind::Warn => write!(f, "warn"),
        }
    }

    fn span(&self) -> Option<proc_macro2::Span> {
        None
    }
}

/// Returns `true` if `field` should generate a `arg` call rather than any other diagnostic
/// call (like `span_label`).
pub(super) fn should_generate_arg(field: &Field) -> bool {
    // Perhaps this should be an exhaustive list...
    field.attrs.iter().all(is_doc_comment)
}
