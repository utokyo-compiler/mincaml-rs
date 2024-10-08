use std::path::PathBuf;

use clap::Parser;
use runtime::run_bytes;

#[derive(Parser, Debug)]
struct CommandLine {
    #[clap(long, value_name = "FILE")]
    /// input file
    input: PathBuf,
}

fn main() {
    let command_line = CommandLine::parse();
    let input_path = command_line.input;

    let input = std::fs::read_to_string(input_path).unwrap();

    run_bytes(input.as_bytes(), std::env::args().skip(1)).unwrap();
}
