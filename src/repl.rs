use clap::{App, Arg};
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::compiler;
use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::vm::Vm;

pub struct Args {
    pub file: Option<String>,
    pub flags: Vec<String>,
}

impl Args {
    pub fn new() -> Self {
        let app = App::new("script-lang")
            .about("An awesome cli for my new scripting language")
            .version("0.0.1")
            .arg(
                Arg::with_name("FILE.kaon")
                    .help("Path to file. If no file is provided interactive mode is run instead")
                    .index(1),
            );
        let matches = app.get_matches();

        Args {
            file: matches
                .value_of("FILE.kaon")
                .and_then(|x| Some(x.to_string())),
            flags: vec![],
        }
    }
}

pub fn start_repl() {
    let mut rl = Editor::<()>::new();

    let mut compiler = Compiler::build();
    let mut vm = Vm::new();

    loop {
        let readline = rl.readline("> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history("history.txt").unwrap();

                let mut parser = Parser::new(line);
                let ast = parser.parse();

                let code = compiler.run(&ast);
                match code {
                    Ok(val) => vm.run(val),
                    Err(compiler::CompileErr(str)) => println!("{}", str),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }
}
