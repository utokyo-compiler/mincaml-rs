use std::path::PathBuf;

use clap::Parser;
use runtime::run_bytes;

#[derive(Parser, Debug)]
struct CommandLine {
    #[clap(value_name = "FILE")]
    /// input file
    input: PathBuf,

    #[arg(last = true)]
    slop: Vec<String>,
}

fn main() {
    let command_line = CommandLine::parse();
    let input_path = command_line.input;

    let input = std::fs::read(input_path).unwrap();

    run_bytes(&input, command_line.slop.into_iter()).unwrap();
}
