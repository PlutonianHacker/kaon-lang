use crate::lexer::Lexer;
use crate::lexer::Source;
use crate::lexer::SyntaxError;

mod lexer;
mod token;

fn lex() {
    let input = r#"1 + 2 * 78 / 31 + 5 - 469"#;
    let source = Source::new(input);
    let mut lexer = Lexer::new(source);
    match lexer.tokenize() {
        Ok(tokens) => {
            println!("[");
            for token in tokens {
                println!("  {:?}", token);
            }
            println!("]");
        }
        Err(SyntaxError(err)) => {
            println!("{:?}", err);
        }
    }
}

fn main() {
    lex();
}
