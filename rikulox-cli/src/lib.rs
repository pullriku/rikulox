use std::{
    cell::RefCell,
    io::{self, Write},
    path::Path,
    rc::Rc,
};

use rikulox_lex::scan::{ScanTokens, Scanner};
use rikulox_parse::parse::Parser;
use rikulox_treewalk::{env::Environment, interp::TreeWalkInterpreter};

struct Runner<'src> {
    env: Rc<RefCell<Environment<'src>>>,
}

impl<'src> Runner<'src> {
    fn run(&mut self, source: &'src str) -> io::Result<()> {
        let ScanTokens {
            tokens,
            eof_span,
            errors: lex_errors,
        } = {
            let mut lexer: Scanner<'src> = Scanner::new(source);
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

        let mut interp: TreeWalkInterpreter<'src> =
            TreeWalkInterpreter::new(Rc::clone(&self.env));

        if let Err(error) = interp.interpret(ast) {
            println!("{error:?}");
        };

        Ok(())
    }
}

pub fn run_file(path: &Path) -> io::Result<()> {
    let source = std::fs::read_to_string(path)?;

    let mut runner = Runner {
        env: Rc::new(RefCell::new(Environment::default())),
    };

    runner.run(&source)?;

    Ok(())
}

pub fn run_repl() -> io::Result<()> {
    let mut runner = Runner {
        env: Rc::new(RefCell::new(Environment::default())),
    };

    loop {
        let mut line = String::new();
        print!("> ");
        io::stdout().flush()?;

        let read_result = io::stdin().read_line(&mut line);

        match read_result {
            Ok(0) => break,
            Ok(..) => {}
            Err(e) => return Err(e),
        }

        runner.run(line.leak())?;
    }

    println!();

    Ok(())
}
