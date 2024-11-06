use std::{
    borrow::Cow,
    error::{Error, Report},
};

use fluent_bundle::{
    resolver::errors::{ReferenceKind, ResolverError},
    FluentArgs, FluentError,
};

use crate::{DiagArg, DiagMessage, FluentBundle};

#[derive(Debug)]
pub struct TranslateError<'args> {
    pub id: &'args Cow<'args, str>,
    pub args: &'args FluentArgs<'args>,
    pub kind: TranslateErrorKind<'args>,
}

impl<'args> TranslateError<'args> {
    pub fn message(id: &'args Cow<'args, str>, args: &'args FluentArgs<'args>) -> Self {
        Self {
            id,
            args,
            kind: TranslateErrorKind::MessageMissing,
        }
    }

    pub fn attribute(
        id: &'args Cow<'args, str>,
        args: &'args FluentArgs<'args>,
        attr: &'args str,
    ) -> Self {
        Self {
            id,
            args,
            kind: TranslateErrorKind::AttributeMissing { attr },
        }
    }

    pub fn value(id: &'args Cow<'args, str>, args: &'args FluentArgs<'args>) -> Self {
        Self {
            id,
            args,
            kind: TranslateErrorKind::ValueMissing,
        }
    }

    pub fn fluent(
        id: &'args Cow<'args, str>,
        args: &'args FluentArgs<'args>,
        errs: Vec<FluentError>,
    ) -> Self {
        Self {
            id,
            args,
            kind: TranslateErrorKind::Fluent { errs },
        }
    }
}

#[derive(Debug)]
pub enum TranslateErrorKind<'args> {
    MessageMissing,
    AttributeMissing { attr: &'args str },
    ValueMissing,
    Fluent { errs: Vec<FluentError> },
}

impl std::fmt::Display for TranslateError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TranslateErrorKind::*;
        let Self { id, args, kind } = self;
        writeln!(f, "failed while formatting fluent string `{id}`: ")?;
        match kind {
            MessageMissing => writeln!(f, "message was missing")?,
            AttributeMissing { attr } => {
                writeln!(f, "the attribute `{attr}` was missing")?;
                writeln!(f, "help: add `.{attr} = <message>`")?;
            }
            ValueMissing => writeln!(f, "the value was missing")?,
            Fluent { errs } => {
                for err in errs {
                    if let FluentError::ResolverError(ResolverError::Reference(
                        ReferenceKind::Message { id, .. } | ReferenceKind::Variable { id, .. },
                    )) = err
                    {
                        if args.iter().any(|(arg_id, _)| arg_id == id) {
                            writeln!(f, "argument `{id}` exists but was not referenced correctly")?;
                            writeln!(f, "help: try using `{{${id}}}` instead")?;
                        } else {
                            writeln!(
                                f,
                                "the fluent string has an argument `{id}` that was not found."
                            )?;
                            let vars: Vec<&str> = args.iter().map(|(a, _v)| a).collect();
                            match &*vars {
                                [] => writeln!(f, "help: no arguments are available")?,
                                [one] => writeln!(f, "help: the argument `{one}` is available")?,
                                [first, middle @ .., last] => {
                                    write!(f, "help: the arguments `{first}`")?;
                                    for a in middle {
                                        write!(f, ", `{a}`")?;
                                    }
                                    writeln!(f, " and `{last}` are available")?;
                                }
                            }
                        }
                    } else {
                        writeln!(f, "{err}")?
                    }
                }
            }
        }
        Ok(())
    }
}

impl Error for TranslateError<'_> {}

pub trait Translate {
    fn fluent_bundle(&self) -> &FluentBundle;

    /// Convert `DiagMessage`s to a string, performing translation if necessary.
    fn translate_messages(&self, messages: &[DiagMessage], args: &FluentArgs<'_>) -> Cow<'_, str> {
        Cow::Owned(
            messages
                .iter()
                .map(|m| {
                    self.translate_message(m, args)
                        .map_err(Report::new)
                        .unwrap()
                })
                .collect::<String>(),
        )
    }

    /// Convert a `DiagMessage` to a string, performing translation if necessary.
    fn translate_message<'dcx: 'args, 'args>(
        &'dcx self,
        message: &'args DiagMessage<'dcx>,
        args: &'args FluentArgs<'args>,
    ) -> Result<Cow<'dcx, str>, TranslateError<'args>> {
        let (identifier, attribute) = match message {
            DiagMessage::Str(cow) | DiagMessage::Translated(cow) => return Ok(cow.clone()),
            DiagMessage::FluentIdentifier {
                identifier,
                attribute,
            } => (identifier, attribute),
        };
        let bundle = self.fluent_bundle();
        let message = bundle
            .get_message(identifier)
            .ok_or(TranslateError::message(identifier, args))?;
        let pattern = match attribute {
            Some(attr) => message
                .get_attribute(attr)
                .ok_or(TranslateError::attribute(identifier, args, attr))?
                .value(),
            None => message
                .value()
                .ok_or(TranslateError::value(identifier, args))?,
        };

        let mut errs = vec![];
        let translated = bundle.format_pattern(pattern, Some(args), &mut errs);
        if errs.is_empty() {
            Ok(translated)
        } else {
            Err(TranslateError::fluent(identifier, args, errs))
        }
    }
}

pub fn to_fluent_args<'iter>(iter: impl Iterator<Item = DiagArg<'iter>>) -> FluentArgs<'static> {
    let mut args = if let Some(size) = iter.size_hint().1 {
        FluentArgs::with_capacity(size)
    } else {
        FluentArgs::new()
    };

    for (k, v) in iter {
        args.set(k.clone(), v.clone());
    }

    args
}
