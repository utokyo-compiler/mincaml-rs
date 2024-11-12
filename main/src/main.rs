use std::{fs, path::PathBuf, process::Termination};

use clap::Parser;
use driver::IntoArgs;
use serde::Deserialize;

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

impl IntoArgs for Config {
    fn into_args(self) -> driver::CompilerArgs {
        driver::CompilerArgs {
            input: self.input,
            output: self.output,
            inline_size_limit: self.inline_size_limit,
        }
    }
}

fn main() -> impl Termination {
    let command_line = CommandLine::parse();

    let config = if let Some(path) = command_line.config {
        let config = fs::read_to_string(path).unwrap();
        toml::from_str(&config).unwrap()
    } else {
        Config::default()
    };
    let config = command_line.command_line.merge(config);

    driver::run_compiler(config.into_args())
}
