use std::{
    cell::RefCell,
    io::{self, BufRead, BufReader, Write},
    mem,
    path::Path,
    rc::Rc,
};

use rikulox_lex::scan::{ScanTokens, Scanner};
use rikulox_parse::parse::Parser;
use rikulox_treewalk::{env::Environment, interp::TreeWalkInterpreter};
use string_interner::{DefaultBackend, StringInterner};

struct Runner {
    string_interner: StringInterner<DefaultBackend>,
    env: Rc<RefCell<Environment>>,
}

impl Runner {
    fn run(&mut self, source: &str) -> io::Result<()> {
        let ScanTokens {
            tokens,
            eof_span,
            errors: lex_errors,
        } = {
            let mut lexer = Scanner::new(source, &mut self.string_interner);
            lexer.scan_tokens()
        };

        for error in &lex_errors {
            println!("{error:?}");
        }

        let mut parser = Parser::new(tokens.into_iter().peekable(), eof_span);
        let parse_result = parser.parse();

        let ast = match parse_result {
            Ok(ast) => ast,
            Err(error) => {
                println!("{error:?}");
                return Ok(());
            }
        };

        dbg!(&ast);

        if !lex_errors.is_empty() {
            return Ok(());
        }

        let mut interp = TreeWalkInterpreter::new(
            mem::take(&mut self.string_interner),
            Rc::clone(&self.env),
        );

        if let Err(error) = interp.interpret(ast) {
            println!("{error:?}");
        };

        let string_interner = interp.into_interner();
        self.string_interner = string_interner;

        Ok(())
    }
}

pub fn run_file(path: &Path) -> io::Result<()> {
    let source = std::fs::read_to_string(path)?;

    let mut runner = Runner {
        env: Rc::new(RefCell::new(Environment::default())),
        string_interner: StringInterner::default(),
    };

    runner.run(&source)?;

    Ok(())
}

pub fn run_repl() -> io::Result<()> {
    let mut reader = BufReader::new(io::stdin());
    let mut line = String::new();
    let mut runner = Runner {
        env: Rc::new(RefCell::new(Environment::default())),
        string_interner: StringInterner::default(),
    };

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

        runner.run(&line)?;
    }

    println!();

    Ok(())
}
