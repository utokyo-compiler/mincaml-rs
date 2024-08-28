use std::{fs, path::PathBuf};

use bumpalo::Bump;
use clap::Parser;
use serde::Deserialize;
use sourcemap::MultipleInputFiles;

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
        .unwrap_or_else(|| exit_with("input files are required"));
    let _output_path = config
        .output
        .unwrap_or_else(|| exit_with("output file is required"));
    let _inline_size_limit = config.inline_size_limit.unwrap_or(1000);

    let mut input_contents = Vec::new();
    for path in input_pathes {
        let content = fs::read_to_string(path).unwrap();
        input_contents.push(content);
    }
    let files = MultipleInputFiles::new(input_contents);
    let input = files.concatenated();

    let bump = Bump::new();

    let _expr = parser::lex_and_parse(&bump, &input).unwrap();
    todo!()
}

fn exit_with(message: &'static str) -> ! {
    <CommandLine as clap::CommandFactory>::command()
        .error(clap::error::ErrorKind::MissingRequiredArgument, message)
        .exit();
}
