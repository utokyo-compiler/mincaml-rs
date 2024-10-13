use std::path::PathBuf;

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

pub struct MultipleInput {
    files: Vec<InputFile>,
    offsets: Vec<usize>,
    offset_accumulated: usize,
}

impl MultipleInput {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            offsets: Vec::new(),
            offset_accumulated: 0,
        }
    }

    pub fn add_file(&mut self, file: InputFile) {
        self.offsets.push(self.offset_accumulated);
        self.offset_accumulated += file.content().chars().count();
        self.files.push(file);
    }

    pub fn concatenated_string(&self) -> String {
        let vec: Vec<_> = self.files.iter().map(|file| file.content()).collect();
        vec.concat()
    }

    pub fn get(&self, loc: sourcemap::Loc) -> Option<(&'_ InputFile, sourcemap::Loc)> {
        let idx = self
            .offsets
            .binary_search(&loc.char_pos)
            .unwrap_or_else(|idx| idx - 1);
        Some((
            &self.files[idx],
            sourcemap::Loc::new(loc.char_pos - self.offsets[idx]),
        ))
    }
}

impl Default for MultipleInput {
    fn default() -> Self {
        Self::new()
    }
}

pub enum InputFile {
    File { path: PathBuf, content: String },
    String { content: String },
}

impl InputFile {
    pub fn content(&self) -> &str {
        match self {
            InputFile::File { content, .. } => content,
            InputFile::String { content } => content,
        }
    }
}

#[derive(Default)]
pub struct CompilerOption {
    pub inline_size_limit: Option<usize>,
}
