#![allow(dead_code)]

use std::fs;

extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

extern crate clap;

use clap::{App, Arg};

use lexer::Lexer;

mod lexer;

struct Args {
    file: Option<String>,
    flags: Vec<String>,
}

impl Args {
    pub fn new() -> Self {
        let app = App::new("script-lang")
            .about("An awesome cli for my new scripting language")
            .version("0.0.1")
            .arg(
                Arg::with_name("FILE")
                    .help("Path to file. If no file is provided interactive mode is run instead")
                    .index(1),
            );
        let matches = app.get_matches();

        Args {
            file: matches.value_of("FILE").and_then(|x| Some(x.to_string())),
            flags: vec![],
        }
    }
}

fn start_repl() {
    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline("> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history("history.txt").unwrap();
                println!("{}", line);
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

fn read_file(path: String) {
    let file = fs::read_to_string(path);
    match file {
        Ok(src) => {
            println!("{}", src);
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
