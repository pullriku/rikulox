use std::{io, path::PathBuf};

use clap::Parser;
use rikulox_cli::{run_file, run_repl};

#[derive(clap::Parser)]
#[clap(
    name = env!("CARGO_PKG_NAME"),
    version = env!("CARGO_PKG_VERSION"),
    author = env!("CARGO_PKG_AUTHORS"),
    about = env!("CARGO_PKG_DESCRIPTION"),
)]
struct Cli {
    path: Option<PathBuf>,
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    match cli.path {
        Some(path) => run_file(&path)?,
        None => run_repl()?,
    }

    Ok(())
}
