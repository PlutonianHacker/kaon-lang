use crate::parser::ParserErr;
use crate::token::Token;
use std::path::PathBuf;

use crate::lexer::Lexer;
use crate::lexer::Source;
use crate::lexer::SyntaxError;
use crate::parser::Parser;

mod lexer;
mod token;
mod parser;
mod ast;
mod data;

fn lex() -> Result<Vec<Token>, String> {
    let input = r#"
        1 + 23 * 5
        10 / 8 - 6
        5 * 2
        6
    "#;
    let source = Source::new(input, &PathBuf::from("./hello.kaon"));
    let mut lexer = Lexer::new(source);
    match lexer.tokenize() {
        Ok(tokens) => {
            println!("Tokens: [");
            for token in &tokens {
                println!("  {:?}", token);
            }
            println!("]");
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
    let tokens = lex().unwrap();
    parse(tokens);
}
