use std::fs;

use kaon_lang::repl::start_repl;
use kaon_lang::repl::Args;

use kaon_lang::compiler;
use kaon_lang::compiler::Compiler;

use kaon_lang::parser::Parser;
use kaon_lang::parser::ParserErr;

use kaon_lang::vm::Vm;

fn read_file(path: String) {
    let file = fs::read_to_string(path);
    match file {
        Ok(src) => {
            /*let mut compiler = Compiler::build();
            let mut vm = Vm::new();

            let mut parser = Parser::new(src);
            let ast = parser.parse();
            match ast {
                Ok(val) => match compiler.run(&val) {
                    Ok(val) => vm.run(val),
                    Err(compiler::CompileErr(str)) => println!("{}", str),
                },
                Err(ParserErr(str)) => {
                    println!("{}", str);
                }
            }*/
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}

fn main() {
    let args = Args::new();

    match args.file {
        Some(path) => {
            read_file(path);
        }
        None => {
            start_repl();
        }
    }
}
