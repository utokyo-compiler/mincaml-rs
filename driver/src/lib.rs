use std::{fs, io::Write, process::ExitCode};

use errors::{DiagMessage, EarlyDiagContext, EarlyDiagnosticEmitter};
use middleware::{Arena, GlobalContext};

use fluent_generated as fluent;

macros::fluent_messages! { "../messages.ftl" }

pub static DEFAULT_LOCALE_RESOURCES: &[&str] = &[crate::DEFAULT_LOCALE_RESOURCE];

pub struct CompilerArgs {
    pub input: Option<Vec<std::path::PathBuf>>,
    pub output: Option<std::path::PathBuf>,
    pub inline_size_limit: Option<usize>,
}

pub trait IntoArgs {
    fn into_args(self) -> CompilerArgs;
}

pub fn run_compiler(at_args: CompilerArgs) -> ExitCode {
    let fluent_bundle = errors::new_fluent_bundle(DEFAULT_LOCALE_RESOURCES.to_vec());
    let early_dcx = EarlyDiagContext::new(EarlyDiagnosticEmitter::new(fluent_bundle));

    let mut fatal_errs = 0;
    let mut fatal = |msg: DiagMessage<'static>| {
        fatal_errs += 1;
        early_dcx.struct_err(msg)
    };

    let mut ml_input = sourcemap::MultipleInput::new();
    let mut mli_input = sourcemap::MultipleInput::new();
    for path in at_args.input.into_iter().flatten() {
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("ml") => {
                ml_input.add_file(sourcemap::InputFile::File {
                    content: fs::read_to_string(&path).unwrap(),
                    path,
                });
            }
            Some("mli") => {
                mli_input.add_file(sourcemap::InputFile::File {
                    content: fs::read_to_string(&path).unwrap(),
                    path,
                });
            }
            Some(ext) => {
                fatal(fluent::driver_unsupported_ext)
                    .with_arg("ext", ext)
                    .emit();
            }
            None => {
                fatal(fluent::driver_missing_ext)
                    .with_arg("path", &path)
                    .with_note(fluent::_subdiag::note)
                    .with_help(fluent::_subdiag::help)
                    .emit();
            }
        }
    }

    if ml_input.is_empty() {
        fatal(fluent::driver_required_input_files).emit();
    }
    if at_args.output.is_none() {
        fatal(fluent::driver_required_output_files).emit();
    }
    if fatal_errs > 0 {
        early_dcx
            .struct_err(fluent::driver_cannot_start)
            .with_arg("errs", fatal_errs)
            .emit();
        return ExitCode::FAILURE;
    }

    let compiler_option = session::CompilerOption {
        inline_size_limit: at_args.inline_size_limit,
    };
    let session = session::Session::new(ml_input, mli_input, at_args.output, compiler_option);

    let arena = Arena::default();
    let global_ctxt = GlobalContext::new(&arena, &session, early_dcx);

    let input_interface = global_ctxt.session().input_interface.concatenated_string();
    let parsed_interface =
        parser::lex_and_parse_mli(global_ctxt.parsing_context(), &input_interface).unwrap();
    let input = global_ctxt.session().input.concatenated_string();
    let parsed_tree = parser::lex_and_parse(global_ctxt.parsing_context(), &input).unwrap();
    let typed_tree = typing::typeck(
        global_ctxt.typing_context(),
        &global_ctxt.common_types,
        parsed_tree,
        parsed_interface,
        global_ctxt.typed_interface(),
    )
    .unwrap();
    let knorm_tree = ir_knorm::lowering(global_ctxt.knorm_context(), typed_tree);
    let closure_prog = ir_closure::lowering(global_ctxt.closure_context(), knorm_tree);
    let wasm_bytes = codegen_wasm::codegen(closure_prog, global_ctxt.typed_interface()).unwrap();
    std::fs::File::create(global_ctxt.session().output_path.as_ref().unwrap())
        .unwrap()
        .write_all(&wasm_bytes)
        .unwrap();

    ExitCode::SUCCESS
}
