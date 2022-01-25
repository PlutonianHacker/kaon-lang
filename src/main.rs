use kaon_lang::repl::start_repl;
use kaon_lang::repl::Args;

use kaon_lang::common::{Source};
use kaon_lang::compiler::compiler;
use kaon_lang::compiler::{Compiler, Lexer, Parser, SemanticAnalyzer};
use kaon_lang::error::{Emitter, SyntaxError};
use kaon_lang::vm::Vm;

fn read_file(path: String) -> Result<(), SyntaxError> {
    let source = Source::from_file(&path);

    match source {
        Ok(src) => {
            let mut compiler = Compiler::build();
            let mut analyzer = SemanticAnalyzer::new();

            let tokens = Lexer::new(src).tokenize()?;

            let ast = Parser::new(tokens).parse(&mut analyzer)?;

            match compiler.run(&ast, analyzer.current_scope) {
                Ok(val) => {
                    //kaon_lang::common::Disassembler::new(&val.name, &val.chunk).disassemble();
                    let mut vm = Vm::new();
                    vm.interpret(val);
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
                Emitter::emit(vec![err.report()]);
            }
            Ok(_) => {}
        },
        None => {
            start_repl();
        }
    }
}
