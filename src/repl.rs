use clap::{App, Arg};
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::analysis::SemanticAnalyzer;
use crate::compiler;
use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::parser::ParserErr;
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

fn print_str(val: &f64) {
    if val % 1.0 == 0.0 {
        println!("{:?}", val.clone() as i64);
    } else {
        println!("{:?}", val);
    }
}

pub fn start_repl() {
    let mut rl = Editor::<()>::new();

    let mut compiler = Compiler::build();
    let mut vm = Vm::new();

    let mut analyzer = SemanticAnalyzer::new();

    loop {
        let readline = rl.readline("> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history("history.txt").unwrap();

                let mut parser = Parser::new(line);
                let ast = parser.parse(&mut analyzer);

                match ast {
                    Ok(val) => match compiler.run(&val) {
                        Ok(val) => {
                            vm.run(val);
                            //print_str(vm.stack.peek());
                            println!("{:?}", vm.stack.peek());
                        }
                        Err(compiler::CompileErr(str)) => println!("{}", str),
                    },
                    Err(ParserErr(str)) => {
                        println!("{}", str);
                    }
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
