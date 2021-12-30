use std::path::PathBuf;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use kaon_lang::parser::ParserErr;
use kaon_lang::token::Token;

use kaon_lang::ast::AST;
use kaon_lang::compiler::Compiler;
use kaon_lang::lexer::Lexer;
use kaon_lang::lexer::SyntaxError;
use kaon_lang::parser::Parser;
use kaon_lang::source::Source;
use kaon_lang::vm::Vm;

fn lex(input: String) -> Result<Vec<Token>, String> {
    let source = Source::new(&input, &PathBuf::from("./hello.kaon"));
    let mut lexer = Lexer::new(source);
    match lexer.tokenize() {
        Ok(tokens) => {
            /*println!("Tokens: [");
            for token in &tokens {
                println!("  {:?}", token);
            }
            println!("]");*/
            return Ok(tokens);
        }
        Err(SyntaxError(err)) => {
            return Err(err);
        }
    }
}

fn parse(tokens: Vec<Token>) -> Result<Vec<AST>, String> {
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    match ast {
        Err(ParserErr(err)) => {
            println!("{}", err);
            return Err(err);
        }
        Ok(ast) => {
            /*println!("AST: [");
            for node in &ast {
                println!("  {:?}", node);
            }
            println!("]");*/
            return Ok(ast);
        }
    }
}

fn main() {
    let mut rl = Editor::<()>::new();

    let mut compiler = Compiler::new();
    let mut vm = Vm::new();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let tokens = lex(line);
                match tokens {
                    Ok(tokens) => {
                        let ast = parse(tokens).unwrap();
                        let chunk = compiler.compile(ast);
                        vm.run(chunk.clone());
                        match vm.stack.peek() {
                            Some(data) => println!("{:?}", data),
                            None => {}
                        };
                    }
                    Err(err) => println!("{}", err),
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
            }
        }
    }
}
