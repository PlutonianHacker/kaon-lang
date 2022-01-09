use std::fs;
use std::path::PathBuf;

use kaon_lang::repl::start_repl;
use kaon_lang::repl::Args;

use kaon_lang::compiler;
use kaon_lang::compiler::Compiler;

use kaon_lang::analysis::SemanticAnalyzer;
use kaon_lang::error::SyntaxError;
use kaon_lang::lexer::Lexer;
use kaon_lang::parser::Parser;
use kaon_lang::source::Source;
use kaon_lang::vm::Vm;

fn read_file(path: String) -> Result<(), SyntaxError> {
    let file = fs::read_to_string(&path);
    match file {
        Ok(src) => {
            let mut compiler = Compiler::build();
            let mut vm = Vm::new();
            let mut analyzer = SemanticAnalyzer::new();

            let source = Source::new(&src, &PathBuf::from(&path));
            let tokens = Lexer::new(source).tokenize()?;

            let ast = Parser::new(tokens).parse(&mut analyzer)?;

            //println!("{:#?}", &ast);

            match compiler.run(&ast) {
                Ok(val) => { 
                    vm.run(val);
                    //println!("{:#?}", &vm.stack);
                    //println!("{:#?}", &vm.chunk);
                }
                Err(compiler::CompileErr(str)) => println!("{}", str),
            }
            Ok(())
        }
        Err(err) => {
            println!("{}", err);
            Ok(())
        }
    }
}

fn main() {
    let args = Args::new();

    match args.file {
        Some(path) => match read_file(path) {
            Err(err) => {
                println!("{}", err);
            }
            Ok(_) => {}
        },
        None => {
            start_repl();
        }
    }
}
