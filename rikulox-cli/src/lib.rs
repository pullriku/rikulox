use std::{
    io::{self, Write},
    path::Path,
};

use rikulox_lex::scan::{ScanTokens, Scanner};
use rikulox_parse::parse::Parser;
use rikulox_resolve::resolve::Resolver;
use rikulox_treewalk::interp::TreeWalkInterpreter;

pub fn run_file(path: &Path) -> io::Result<()> {
    let source = std::fs::read_to_string(path)?;

    let mut interp = TreeWalkInterpreter::new();

    run(&source, &mut interp)?;

    Ok(())
}

pub fn run_repl() -> io::Result<()> {
    let mut interp: TreeWalkInterpreter = TreeWalkInterpreter::new();

    loop {
        let mut line = String::new();
        print!("> ");
        io::stdout().flush()?;

        let read_result = io::stdin().read_line(&mut line);
        let line = line.leak();

        match read_result {
            Ok(0) => break,
            Ok(..) => {}
            Err(e) => return Err(e),
        }

        run(line, &mut interp)?;
    }

    println!();

    Ok(())
}

fn run<'src>(
    source: &'src str,
    interp: &mut TreeWalkInterpreter<'src>,
) -> io::Result<()> {
    let ScanTokens {
        tokens,
        eof_span,
        errors: lex_errors,
    } = {
        let mut lexer = Scanner::new(source);
        lexer.scan_tokens()
    };

    if !lex_errors.is_empty() {
        println!("{lex_errors:#?}");
    }

    let mut parser = Parser::new(tokens.into_iter().peekable(), eof_span);
    let parse_result = parser.parse();

    let ast = match &parse_result {
        Ok(ast) => ast,
        Err(error) => {
            println!("{error:?}");
            return Ok(());
        }
    };

    if !lex_errors.is_empty() {
        return Ok(());
    }

    let mut resolver = Resolver::new();
    let resolve_result = resolver.resolve(ast);

    if let Err(error) = &resolve_result {
        println!("{error:?}");
    }

    if !lex_errors.is_empty()
        || parse_result.is_err()
        || resolve_result.is_err()
    {
        return Ok(());
    }

    if let Err(error) = interp.interpret(ast, resolver.into_locals()) {
        println!("{error:?}");
    };

    Ok(())
}
