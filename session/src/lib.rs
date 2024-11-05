use std::path::PathBuf;

use sourcemap::MultipleInput;

/// Session is a struct that holds the input and compiler options for a single compilation.
pub struct Session {
    pub input: MultipleInput,
    pub input_interface: MultipleInput,
    pub output_path: Option<PathBuf>,
    pub compiler_option: CompilerOption,
}

impl Session {
    pub fn new(
        input: MultipleInput,
        input_interface: MultipleInput,
        output_path: Option<PathBuf>,
        compiler_option: CompilerOption,
    ) -> Self {
        Self {
            input,
            input_interface,
            output_path,
            compiler_option,
        }
    }
}

#[derive(Default)]
pub struct CompilerOption {
    pub inline_size_limit: Option<usize>,
}
