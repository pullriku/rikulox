use std::{
    io::{self, BufRead, BufReader, Write},
    path::Path,
};

use rikulox_parse::parse::Parser;
use rikulox_scan::scan::{ScanTokens, Scanner};
use string_interner::StringInterner;

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

        run(line.trim_end());
    }

    println!();

    Ok(())
}

fn run(source: &str) {
    let mut string_interner = StringInterner::default();
    let mut scanner = Scanner::new(source, &mut string_interner);
    let ScanTokens {
        tokens,
        eof_span,
        errors,
    } = scanner.scan_tokens();

    for error in errors {
        println!("{error:?}");
    }

    println!("tokens:");
    for token in &tokens {
        println!("{:#?}", token.kind);
    }

    let mut parser = Parser::new(tokens.into_iter().peekable(), eof_span);
    let parse_result = parser.parse();

    println!("{parse_result:#?}");
}
