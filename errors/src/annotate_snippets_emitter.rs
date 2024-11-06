use annotate_snippets::{Annotation, Renderer};
use data_structure::{arena::TypedArena, FxIndexMap};
use sourcemap::{InputIndex, MultipleInput};

use crate::{
    translation::{to_fluent_args, Translate},
    DiagInner, FluentBundle, Level,
};

pub struct AnnotateSnippetEmitter<'dcx> {
    fluent_bundle: FluentBundle,
    source_map: &'dcx MultipleInput,
}

impl From<Level> for annotate_snippets::Level {
    fn from(value: Level) -> Self {
        match value {
            Level::Error | Level::Bug => annotate_snippets::Level::Error,
            Level::Info => annotate_snippets::Level::Info,
        }
    }
}

impl Translate for AnnotateSnippetEmitter<'_> {
    fn fluent_bundle(&self) -> &FluentBundle {
        &self.fluent_bundle
    }
}

impl AnnotateSnippetEmitter<'_> {
    pub fn emit_diagnostic(&self, diag: DiagInner) {
        let fluent_args = to_fluent_args(diag.args.iter());

        let title = self.translate_messages(&diag.content.messages, &fluent_args);
        let mut message = annotate_snippets::Level::from(diag.content.level).title(&title);

        // We need to allocate strings for the snippets. We use a `TypedArena` to
        // avoid the need to manually manage the lifetimes of each string.
        let owned_str_repository = TypedArena::<u8>::new();

        /// The original `Snippet` struct does consume `self`, which is inconvenient here.
        struct Snippet<'a> {
            origin: &'a str,
            line_start: usize,

            source: &'a str,
            annotations: Vec<Annotation<'a>>,
        }

        impl<'a> Snippet<'a> {
            pub fn into_snippet(self) -> annotate_snippets::Snippet<'a> {
                annotate_snippets::Snippet::source(self.source)
                    .origin(self.origin)
                    .line_start(self.line_start)
                    .annotations(self.annotations)
            }
        }

        let mut snippets = FxIndexMap::<InputIndex, Snippet<'_>>::with_hasher(<_>::default());

        for (span, label) in diag.content.span.iter_labeled() {
            let mut annotation = if diag.content.span.primary_span.contains(span) {
                annotate_snippets::Level::Error
            } else {
                annotate_snippets::Level::Info
            }
            .span(span.into_range());
            if let Some(label) = label {
                let label = self.translate_message(label, &fluent_args).unwrap();
                annotation = annotation.label(owned_str_repository.alloc_str(&label));
            }
            let index = self.source_map.get_span_index(*span).unwrap();
            let file = &self.source_map.files()[index];
            let source = file.content();
            let line_start = source
                .chars()
                .take(span.start.char_pos)
                .filter(|c| *c == '\n')
                .count();
            snippets
                .entry(index)
                .and_modify(|snippet| {
                    if snippet.line_start > line_start {
                        snippet.line_start = line_start;
                    }
                })
                .or_insert_with(|| {
                    let file = &self.source_map.files()[index];
                    let source = file.content();
                    Snippet {
                        origin: owned_str_repository.alloc_str(&file.origin_string()),
                        line_start,
                        source,
                        annotations: Vec::new(),
                    }
                })
                .annotations
                .push(annotation);
        }

        message = message.snippets(snippets.into_values().map(Snippet::into_snippet));

        let renderer = Renderer::styled();
        anstream::eprintln!("{}", renderer.render(message))
    }
}
