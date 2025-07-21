use std::{
    io::{self, BufRead, BufReader, Write},
    path::Path,
};

use rikulox_scan::scan::Scanner;

pub fn run_file(path: &Path) -> io::Result<()> {
    let source = std::fs::read_to_string(path)?;

    run(&source);

    Ok(())
}

pub fn run_repl() -> io::Result<()> {
    let mut reader = BufReader::new(io::stdin());
    let mut line = String::new();

    loop {
        line.clear();
        print!("> ");
        io::stdout().flush().unwrap();

        let read_result = reader.read_line(&mut line);

        match read_result {
            Ok(0) => break,
            Ok(..) => {}
            Err(e) => return Err(e),
        }

        run(&line);
    }

    println!();

    Ok(())
}

fn run(source: &str) {
    let scanner = Scanner::new(source);
    let (tokens, errors): (Vec<_>, Vec<_>) = scanner.partition(|token| token.is_ok());

    println!("{:?} errors", errors.len());
    for error in errors {
        println!("{:?}", error.unwrap_err());
    }

    println!("{:?} tokens", tokens.len());
    for token in tokens {
        println!("{:?}", token.unwrap());
    }
}
