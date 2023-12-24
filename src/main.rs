use std::path::PathBuf;

use clap::Parser;
use tokio::runtime::Builder;

mod types;
mod parser;
mod machine;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(required = true)]
    file: PathBuf
}

fn main() {
    let args = Cli::parse();

    let program = match parser::parse_file(&args.file) {
	Ok(program) => program,
	Err(parser::ParseError::ErrorReadingFile) => panic!("Error while reading file"),
	Err(parser::ParseError::ErrorParsingFile) => panic!("Error while parsing file"),
    };

    let rt = Builder::new_multi_thread()
	.enable_all()
	.build()
	.expect("Tokio initialization error");
    
    rt.block_on(async {
	let query = types::BasicType::Atom("main".into());
	machine::run(query, &program).await;
    });
}
