use std::env;
use std::fs;
use std::path::PathBuf;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use kaon_lang::ast::AST;
use kaon_lang::compiler::Chunk;
use kaon_lang::compiler::Compiler;
use kaon_lang::lexer::Lexer;
use kaon_lang::parser::Parser;
use kaon_lang::parser::SyntaxError;
use kaon_lang::source::Source;
use kaon_lang::token::Token;
use kaon_lang::vm::Vm;

fn lex(input: String) -> Result<Vec<Token>, SyntaxError> {
    let source = Source::new(&input, &PathBuf::from("./hello.kaon"));
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

fn parse(tokens: Vec<Token>) -> Result<Vec<AST>, SyntaxError> {
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    return ast;
}

fn compile(path: String) -> Result<Chunk, SyntaxError> {
    let tokens = lex(path)?;
    let ast = parse(tokens)?;
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(ast);
    return Ok(chunk.clone());
}

fn main() {
    let args = env::args().collect::<Vec<String>>();

    let mut rl = Editor::<()>::new();
    let mut vm = Vm::new();

    if args.get(1).is_some() {
        let source = fs::read_to_string(&args[1]).expect("Invalid path to file");
        match compile(source) {
            Err(err) => {
                println!("{}", err);
            }
            Ok(code) => {
                vm.run(code.clone());
                match vm.stack.peek() {
                    Some(data) => println!("{:?}", data),
                    None => {}
                };
            }
        }
    } else {
        loop {
            let readline = rl.readline("> ");
            match readline {
                Ok(line) => match compile(line) {
                    Err(err) => {
                        println!("{:?}", err);
                    }
                    Ok(code) => {
                        vm.run(code.clone());
                        match vm.stack.peek() {
                            Some(data) => println!("{:?}", data),
                            None => {}
                        };
                    }
                },
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
}
