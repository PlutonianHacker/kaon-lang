use std::fs;

use kaon_lang::repl::start_repl;
use kaon_lang::repl::Args;

use kaon_lang::compiler;
use kaon_lang::compiler::Compiler;

use kaon_lang::parser::Parser;

use kaon_lang::vm::Vm;

fn read_file(path: String) {
    let file = fs::read_to_string(path);
    match file {
        Ok(src) => {
            let mut parser = Parser::new(src);
            let ast = parser.parse();
            let mut compiler = Compiler::build();

            match compiler.run(&ast) {
                Ok(val) => {
                    let mut vm = Vm::new();
                    vm.run(val);
                }
                Err(compiler::CompileErr(err)) => {
                    println!("{}", err);
                }
            }
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
