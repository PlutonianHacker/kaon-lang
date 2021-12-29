use std::path::PathBuf;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::parser::ParserErr;
use crate::token::Token;

use crate::lexer::Lexer;
use crate::lexer::SyntaxError;
use crate::parser::Parser;
use crate::source::Source;

mod ast;
mod data;
mod lexer;
mod parser;
mod source;
mod token;

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

fn parse(tokens: Vec<Token>) {
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    match ast {
        Err(ParserErr(err)) => {
            println!("{}", err);
        }
        Ok(ast) => {
            println!("AST: [");
            for node in &ast {
                println!("  {:?}", node);
            }
            println!("]");
        }
    }
}

fn main() {
    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let tokens = lex(line);
                match tokens {
                    Ok(tokens) => parse(tokens),
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
