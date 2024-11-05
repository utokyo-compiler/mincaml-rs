use std::{fs, path::PathBuf};

use clap::Parser;
use serde::Deserialize;

mod compiler;

#[derive(Parser, Debug)]
struct CommandLine {
    #[clap(flatten)]
    command_line: Config,
    #[clap(long, value_name = "FILE")]
    /// Path to the config file
    config: Option<PathBuf>,
}

#[derive(clap::Args, Deserialize, Debug, Default)]
struct Config {
    #[clap(short, long, value_name = "FILE")]
    /// Path to the input files in order
    input: Option<Vec<PathBuf>>,
    #[clap(short, long, value_name = "FILE")]
    /// Path to the output file
    output: Option<PathBuf>,
    #[clap(long)]
    /// The maximum size of functions inlined
    inline_size_limit: Option<usize>,
}

impl Config {
    fn merge(self, other: Self) -> Self {
        Self {
            input: self.input.or(other.input),
            output: self.output.or(other.output),
            inline_size_limit: self.inline_size_limit.or(other.inline_size_limit),
        }
    }
}

fn main() {
    let command_line = CommandLine::parse();

    let config = if let Some(path) = command_line.config {
        let config = fs::read_to_string(path).unwrap();
        toml::from_str(&config).unwrap()
    } else {
        Config::default()
    };
    let config = command_line.command_line.merge(config);
    let input_pathes = config
        .input
        .unwrap_or_else(|| exit_missing_args("input files are required"));
    let output_path = config
        .output
        .unwrap_or_else(|| exit_missing_args("output file is required"));

    let mut ml_input = sourcemap::MultipleInput::new();
    let mut mli_input = sourcemap::MultipleInput::new();
    for path in input_pathes {
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("ml") => ml_input.add_file(sourcemap::InputFile::File {
                content: fs::read_to_string(&path).unwrap(),
                path,
            }),
            Some("mli") => mli_input.add_file(sourcemap::InputFile::File {
                content: fs::read_to_string(&path).unwrap(),
                path,
            }),
            _ => panic!("unsupported file extension or missing extension"),
        }
    }

    let compiler_option = session::CompilerOption {
        inline_size_limit: config.inline_size_limit,
    };
    let session = session::Session::new(ml_input, mli_input, Some(output_path), compiler_option);

    compiler::run(session);
}

fn exit_missing_args(message: &'static str) -> ! {
    <CommandLine as clap::CommandFactory>::command()
        .error(clap::error::ErrorKind::MissingRequiredArgument, message)
        .exit();
}
