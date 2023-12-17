use std::path::PathBuf;

use clap::Parser;

mod parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(required = true)]
    file: PathBuf
}

fn main() {
    let args = Cli::parse();

    let program = parser::parse_file(&args.file);
}
